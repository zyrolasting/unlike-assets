#lang racket/base

(require idiocket/path
         unlike-assets/resolver
         web-server/http/request-structs
         web-server/http/response-structs
         file/sha1
         racket/contract
         racket/port
         racket/function
         idiocket/file
         "files/contracts.rkt"
         "files/resolve.rkt")

(provide
 (all-from-out "files/contracts.rkt"
               "files/resolve.rkt")
 (contract-out
  [static-files (->* ((-> complete-path? exact-integer? asset/file-to-file/c))
                     #:rest (listof (or/c directory-exists? (-> string? complete-path?)))
                     ->live-build/c)]
  [file-path->asset (->* (complete-path? complete-path?) (bytes?) asset/file-to-file/c)]
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
         [write copy]
         [->http-response
          (response/output #:mime-type mime-type copy)]))


(define (make-key->path . search-dirs)
  (λ (key)
    (with-handlers ([exn:fail? (λ _ #f)])
      (find-file-path key search-dirs #:must-exist #t))))


; Defines an extension that produces files named after their own content hashes.
; Useful for invalidating agressive web caches that would otherwise keep the
; same information forever.
(define (static-files on-new-file . maybe-key->paths)
  (define key->paths
    (map (λ (variant)
           (if (or (path-string? variant)
                   (path-for-some-system? variant))
               (make-key->path variant)
               variant))
         maybe-key->paths))
  (define (key->path key)
    (ormap (λ (f) (f key)) key->paths))
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
