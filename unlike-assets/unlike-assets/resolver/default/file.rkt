#lang racket/base

(require unlike-assets/resolver
         racket/contract
         idiocket/string
         idiocket/file
         idiocket/path
         "extension.rkt")

(provide
 (contract-out
  [static-files (->* ((-> complete-path? hash-eq?)
                      (-> string? (or/c #f complete-path?)))
                     (-> string? (or/c #f procedure?)))]))

(define (static-files on-new-file key->path)
  (λ (key recurse)
    (define path (key->path key))
    (and path
         (file-exists? path)
         (make-factory-thunk
          (make-fence-thunk (λ () (file-or-directory-modify-seconds path)))
          (λ () (on-new-file path))))))
