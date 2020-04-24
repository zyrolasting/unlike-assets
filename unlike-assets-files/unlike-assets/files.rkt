#lang racket/base

(require unlike-assets/resolver
         web-server/http/request-structs
         web-server/http/response-structs
         file/sha1
         racket/contract
         racket/function
         racket/port
         idiocket/file
         idiocket/path
         "files/contracts.rkt"
         "files/resolve.rkt")

(provide
 (all-from-out "files/contracts.rkt"
               "files/resolve.rkt")
 (contract-out
  [static-files (->* ((-> complete-path? asset?)
                      (-> string? (or/c #f complete-path?)))
                     route/c)]
  [within-directories (-> (-> path? any/c)
                          (or/c (or/c path-string? path-for-some-system?)
                                (non-empty-listof (or/c path-string? path-for-some-system?)))
                          (-> string? (or/c #f path?)))]
  [file-path->asset (->* (complete-path? complete-path?)
                         (#:mime-type bytes?
                          #:writer (or/c #f (-> output-port? (or/c void? exact-nonnegative-integer?))))
                         asset/file-to-file/c)]))

(define default-media #"application/octet-stream")

(define (file-path->asset path
                          output-path
                          #:mime-type [mime-type default-media]
                          #:writer [writer #f])
  (define (copy out)
    (call-with-input-file path
      (λ (in) (copy-port in out))))
  (define write-file (or writer copy))
  (asset [input-file-path path]
         [output-file-path output-path]
         [write-file write-file]
         [->http-response (λ (req) (response/output #:mime-type mime-type write-file))]))

(define (within-directories match? search)
  (define search-dirs (if (list? search) search (list search)))
  (λ (key)
    (define maybe (find-file-path key search-dirs #:must-exist #f))
    (and (match? maybe) maybe)))

(define (static-files on-new-file key->path)
  (λ (key recurse)
    (define path (key->path key))
    (and path
         (file-exists? path)
         (make-pod/fenced
          key
          (fence (file-or-directory-modify-seconds path))
          (λ () (on-new-file path))))))
