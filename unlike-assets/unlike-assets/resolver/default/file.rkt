#lang racket/base

(require unlike-assets/resolver
         racket/contract
         idiocket/string
         idiocket/file
         idiocket/path
         "extension.rkt")

(provide
 (contract-out
  [show-file-info (-> (and/c complete-path? file-exists?) hash-eq?)]
  [search-within  (or/c pathy/c
                        (non-empty-listof pathy/c)
                        (-> pathy/c (or/c #f complete-path?)))]
  [file-modules (->* ((-> (and/c complete-path? file-exists?) hash-eq?)
                      (-> any/c (or/c #f complete-path?)))
                      (-> any/c (or/c #f (-> hash-eq?))))]))

(define (search-within search-dirs [match? file-exists?])
  (within-directories file-exists? search-dirs))

(define (show-file-info path)
  (hasheq 'file-or-directory-identity
          (file-or-directory-identity path)
          'file-or-directory-permissions
          (file-or-directory-permissions path)
          'file-size
          (file-size path)
          'file-or-directory-modify-seconds
          (file-or-directory-modify-seconds path)))

(define (file-modules on-new-file key->path)
  (λ (key recurse)
    (define path (key->path key))
    (and path
         (make-factory-thunk
          (make-fence-thunk (λ () (file-or-directory-modify-seconds path)))
          (λ () (on-new-file path))))))
