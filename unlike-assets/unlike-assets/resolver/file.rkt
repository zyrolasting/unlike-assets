#lang racket/base

(require unlike-assets/resolver
         racket/contract
         idiocket/string
         idiocket/file
         idiocket/path
         "extension.rkt")

(provide
 (contract-out
  [search-within  (->* ((or/c pathy/c
                              (non-empty-listof pathy/c)))
                       ((-> pathy/c any/c))
                       (-> pathy/c (or/c #f complete-path?)))]
  [existing-files (-> (-> complete-path? any/c)
                      (-> any/c (or/c #f complete-path?))
                      (-> any/c
                          resolver?
                          (or/c #f (-> any/c))))]))

(define (search-within search-dirs [match? file-exists?])
  (within-directories file-exists? search-dirs))

(define (existing-files on-new-file key->maybe-complete-path)
  (λ (key recurse)
    (define maybe-complete-path (key->maybe-complete-path key))
    (and maybe-complete-path
         (fenced-factory (file-or-directory-modify-seconds maybe-complete-path)
                         (on-new-file maybe-complete-path)))))
