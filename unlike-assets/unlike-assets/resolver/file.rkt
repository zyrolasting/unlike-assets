#lang racket/base

(require unlike-assets/resolver
         racket/contract
         idiocket/string
         idiocket/file
         idiocket/path
         "../resolver.rkt"
         "thunk.rkt")

(provide make-filesystem-resolver)

(define (make-filesystem-resolver
         on-mtime-change
         #:search-directories [search-directories null]
         #:dependency-relative? [dependency-relative? #t]
         #:allow-directories? [allow-directories? #f]
         #:allow-files? [allow-files? #t]
         #:allow-links? [allow-links? #f])

  (define (verify-resolved-name unresolved-name resolved-name dependents)
    (if (and (complete-path? resolved-name)
             (or (and allow-files? (file-exists? resolved-name))
                 (and allow-directories? (directory-exists? resolved-name))
                 (and allow-links? (link-exists? resolved-name))))
        resolved-name
        (raise-name-resolution-error unresolved-name dependents)))

  (define (verify-unresolved-name unresolved-name dependents)
    (if (path-string? unresolved-name)
        unresolved-name
        (raise-name-resolution-error unresolved-name dependents)))

  (define (make-resolved-name unresolved-name dependents)
    (if (path-string? key)
        (simplify-path
         (cond [(complete-path? key) key]
               [(and (null? dependents)
                     (not (null? search-directories)))
                ((within-directories file-exists? search-directories) key)]
               [(and dependency-relative?
                     (not (null? dependents)))
                (build-path (path-only (car dependents)) key)]))
        key))

  (define (make-thunk resolved-name dependents site)
    (fenced-factory (file-or-directory-modify-seconds resolved-name)
                    (on-mtime-change resolved-name dependents site))))

  (make-resolver make-resolved-name make-thunk))
