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
  [static-files (->* ((-> complete-path? asset/file-to-file/c)
                      (or/c (listof (and/c path-string?
                                           path-for-some-system?
                                           directory-exists?))
                            (-> string? (or/c #f complete-path?))))
                     route/c)]
  [file-path->asset (->* (complete-path? complete-path?) (bytes?) asset/file-to-file/c)]
  [make-cache-busting-file-name (->* (file-exists?) ((or/c input-port? #f)) path?)]))

(define default-media #"application/octet-stream")

; Derives a file's name from it's own content.
(define (make-cache-busting-file-name file-path [port #f])
  (if (input-port? port)
      (path-replace-extension (substring (sha1 port) 0 8)
                              (path-get-extension file-path))
      (call-with-input-file file-path
        (λ (port) (make-cache-busting-file-name file-path port)))))

(define (file-path->asset path output-path [mime-type default-media])
  (define (copy out)
    (call-with-input-file path
      (λ (in) (copy-port in out))))
  (asset [input-file-path path]
         [output-file-path output-path]
         [write-file copy]
         [->http-response
          (response/output #:mime-type mime-type copy)]))

(define (static-files on-new-file variant)
  (define key->path
    (if (procedure? variant)
        variant
        (λ (key)
          (find-file-path key variant #:must-exist #f))))
  (λ (key recurse)
    (define path (key->path key))
    (and path
         (make-pod/fenced
          key
          (λ () (file-or-directory-modify-seconds path))
          (λ () (on-new-file path))))))
