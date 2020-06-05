#lang racket/base

(require racket/contract
         racket/rerequire
         "../resolver.rkt"
         "extension.rkt")

(define module-path/c
  (or/c module-path?
        resolved-module-path?
        module-path-index?))

(provide
 module-path/c
 (contract-out
  [module-path->hasheq (-> module-path/c hash-eq?)]
  [racket-modules (->* ((-> string? (or/c module-path/c #f)))
                       ((-> module-path/c (not/c procedure?)))
                       (-> any/c resolver? (or/c #f (-> any/c))))]))

(define (module-path->hasheq module-path)
  (dynamic-require module-path #f)
  (define-values (exported-variables _) (module->exports module-path))
  (define phase0 (findf (λ (x) (eq? (car x) 0)) exported-variables))
  (for/hasheq ([id (map car (cdr phase0))])
    (values id (dynamic-require module-path id))))

(define (racket-modules make-module-path [make-asset module-path->hasheq])
  (λ (key recurse)
    (let ([module-path (make-module-path key)])
      (and module-path
           (make-factory-thunk
            (make-fence-thunk (λ () (dynamic-rerequire module-path #:verbosity 'none))
                              (λ (a b) (null? b)))
            (λ () (make-asset module-path)))))))
