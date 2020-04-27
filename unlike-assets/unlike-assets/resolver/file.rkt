#lang racket/base

(require unlike-assets/resolver
         racket/contract
         idiocket/string
         idiocket/file
         idiocket/path
         "extension.rkt")

(provide
 (contract-out
  [get-file-info (-> (and/c complete-path? file-exists?) hash-eq?)]
  [search-within  (or/c pathy/c
                        (non-empty-listof pathy/c)
                        (-> pathy/c (or/c #f complete-path?)))]
  [existing-files (->* ((-> complete-path? (not/c procedure?))
                        (-> any/c (or/c #f complete-path?)))
                       (-> any/c (or/c #f (-> (not/c procedure?)))))]))

(define (search-within search-dirs [match? file-exists?])
  (within-directories file-exists? search-dirs))

(define (get-file-info path)
  (hasheq 'file-or-directory-identity
          (file-or-directory-identity path)
          'file-or-directory-permissions
          (file-or-directory-permissions path)
          'file-size
          (file-size path)
          'file-or-directory-modify-seconds
          (file-or-directory-modify-seconds path)))

(define (existing-files on-new-file key->maybe-complete-path)
  (λ (key recurse)
    (define maybe-complete-path (key->maybe-complete-path key))
    (and maybe-complete-path
         (fenced-factory (file-or-directory-modify-seconds maybe-complete-path)
                         (on-new-file maybe-complete-path)))))
