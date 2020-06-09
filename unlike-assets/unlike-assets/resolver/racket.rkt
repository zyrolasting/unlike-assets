#lang racket/base

(require racket/contract
         racket/rerequire
         "../resolver.rkt"
         "thunk.rkt")

(define module-path/c
  (or/c module-path?
        resolved-module-path?
        module-path-index?))

(provide
 module-path/c
 (contract-out
  [module-path->hasheq (->* (module-path/c) (exact-nonnegative-integer?) hash-eq?)]
  [make-racket-module-resolver
   (->* ((-> any/c list? module-path/c))
        ((-> module-path/c any/c) #:reload? any/c #:verbosity (or/c 'all 'reload 'none))
        resolver/c)]))

(define (module-path->hasheq module-path [phase 0])
  (dynamic-require module-path #f)
  (define-values (exported-variables _) (module->exports module-path))
  (define phase0 (findf (λ (x) (eq? (car x) phase)) exported-variables))
  (for/hasheq ([id (map car (cdr phase0))])
    (values id (dynamic-require module-path id))))

(define (make-racket-module-resolver #:reload? [reload? #t]
                                     #:verbosity [verbosity 'none]
                                     make-module-path
                                     [make-value (λ (rn deps seat) (module-path->hasheq rn))])
  (make-resolver make-module-path
                 (λ (resolved-name dependents seat)
                   (define compute (λ () (make-value resolved-name dependents seat)))

                   (if reload?
                       (make-factory-thunk
                        (make-fence-thunk
                         (λ () (dynamic-rerequire resolved-name #:verbosity verbosity))
                         (λ (a b) (null? b)))
                        compute)
                       compute))))
