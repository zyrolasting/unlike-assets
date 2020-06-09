#lang racket/base

(require unlike-assets/resolver
         racket/contract
         idiocket/string
         idiocket/file
         idiocket/path
         "../resolver.rkt"
         "thunk.rkt"
         "exn.rkt")

(provide make-filesystem-resolver)

(define (make-filesystem-resolver make-value
         #:search-directories [search-directories null]
         #:dependency-relative? [dependency-relative? #t]
         #:changed? [changed? file-or-directory-modify-seconds]
         #:must-exist? [must-exist? #t]
         #:allow-directories? [allow-directories? #f]
         #:allow-files? [allow-files? #t]
         #:allow-links? [allow-links? #f])

  (define (verify-resolved-name unresolved-name resolved-name dependents)
    (if (and (complete-path? resolved-name)
             (or (not must-exist?)
                 (and allow-files? (file-exists? resolved-name))
                 (and allow-directories? (directory-exists? resolved-name))
                 (and allow-links? (link-exists? resolved-name))))
        resolved-name
        (raise-name-resolution-error unresolved-name dependents)))


  (define (make-resolved-name unresolved-name dependents)
    (unless (path-string? unresolved-name)
      (raise-name-resolution-error unresolved-name dependents))
    (verify-resolved-name
     (simplify-path
      (cond [(complete-path? unresolved-name) unresolved-name]
            [(and (null? dependents)
                  (not (null? search-directories)))
             ((within-directories file-exists? search-directories)
              unresolved-name)]
            [(and dependency-relative?
                  (not (null? dependents))
                  (complete-path? (car dependents)))
             (build-path (path-only (car dependents))
                         unresolved-name)]))))

  (define (make-thunk resolved-name dependents site)
    (fenced-factory (changed? resolved-name)
                    (make-value resolved-name dependents site)))

  (make-resolver make-resolved-name make-thunk))


#;(module+ test
  (require rackunit
           racket/string)

  (test-case "Using most involved filesystem resolver"
    (define (touch-file base-path rel-path [writer void])
      (define outfile (build-path base-path rel-path))
      (make-directory* outfile)
      (call-with-output-file #:exists 'replace outfile writer))

    (define (path-prefix? base path)
      (string-prefix? (path->string path)
                      (path->string base)))

    (define documents/ (make-temporary-file "rkttmp~a" 'directory))

    (for ([fn (in-list '("resume" "cv" "blog/opinion" "blog/index" "images/headshot"))])
      (touch-file documents/ fn))

    (define site
      (make-site
       (make-filesystem-resolver
        #:search-directories (list documents/ images/)
        #:dependent-relative? #t
        #:sample file-or-directory-modify-seconds
        #:must-exist? #t
        #:allow-directories? #t
        #:allow-files? #t
        #:allow-links? #t
        (λ (path dependents seat)
          (λ () (file-size path))))))

    (dynamic-wind void
                  (λ () (parameterize ([current-site site])
                          (procure "resume")))
                  (λ ()
                    (delete-directory/files documents/)))))
