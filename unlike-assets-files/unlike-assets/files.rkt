#lang racket/base

(require idiocket/path
         unlike-assets/resolver
         web-server/http/request-structs
         web-server/http/response-structs
         file/sha1
         racket/contract
         racket/port
         racket/function
         "files/contracts.rkt"
         "files/resolve.rkt")

(provide
 (all-from-out "files/contracts.rkt"
               "files/resolve.rkt")
 (contract-out
  [static-files (-> (-> string? complete-path?)
                    (-> complete-path? exact-integer? asset/file-to-file/c)
                    ->live-build/c)]
  [file-path->asset (-> complete-path? complete-path? bytes? asset/file-to-file/c)]
  [make-cache-busting-file-name (-> file-exists? path?)]))

(define default-media #"application/octet-stream")

(define (make-cache-busting-file-name file-path)
  (call-with-input-file file-path
    (λ (port)
      (path-replace-extension (substring (sha1 port) 0 8)
                              (path-get-extension file-path)))))

(define (file-path->asset path output-path [mime-type default-media])
  (define (copy out)
    (call-with-input-file path
      (λ (in) (copy-port in out))))
  (asset [input-file-path path]
         [output-file-path output-path]
         [write-all-bytes copy]
         [->http-response
          (response/output #:mime-type mime-type copy)]))

; Defines an extension that produces files named after their own content hashes.
; Useful for invalidating agressive web caches that would otherwise keep the
; same information forever.
(define (static-files key->path on-new-file)
  (λ (key recurse)
    (define path (key->path key))
    (and path
         (file-exists? path)
         (start-live-build! key
                            #:suppress? equal?
                            #:sample!
                            (λ () (file-or-directory-modify-seconds path))
                            #:build!
                            (λ (mtime) (on-new-file path mtime))))))
