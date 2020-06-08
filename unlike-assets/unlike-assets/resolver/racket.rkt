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
  [module-path->hasheq (-> module-path/c hash-eq?)]
  [make-racket-module-resolver
   (->* ((-> any/c module-path/c))
        ((-> module-path/c any/c))
        resolver/c)]))

(define (module-path->hasheq module-path)
  (dynamic-require module-path #f)
  (define-values (exported-variables _) (module->exports module-path))
  (define phase0 (findf (λ (x) (eq? (car x) 0)) exported-variables))
  (for/hasheq ([id (map car (cdr phase0))])
    (values id (dynamic-require module-path id))))

(define (make-racket-module-resolver make-module-path [make-value module-path->hasheq])
  (make-resolver make-module-path
                 (λ (resolved-name dependents seat)
                   (make-factory-thunk
                    (make-fence-thunk (λ () (dynamic-rerequire resolved-name #:verbosity 'none))
                                      (λ (a b) (null? b)))
                    (λ () (make-value resolved-name))))))
