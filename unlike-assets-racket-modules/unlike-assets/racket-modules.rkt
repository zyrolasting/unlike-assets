#lang racket/base

(require racket/contract
         racket/function
         racket/rerequire
         racket/path
         racket/sequence
         unlike-assets/resolver
         unlike-assets/files)

(define module-path-like/c
  (or/c path-for-some-system?
        module-path?
        resolved-module-path?
        module-path-index?))

(provide
 module-path-like/c
 (contract-out
  [make-asset-from-provides (-> module-path-like/c asset?)]
  [racket-modules (->* ((-> string? (or/c path-string? module-path-like/c #f)))
                       ((-> module-path-like/c asset?))
                       procedure?)]))

(define (make-asset-from-provides module-path)
  (dynamic-require module-path (void))
  (define-values (exported-variables _) (module->exports module-path))
  (define phase0 (findf (λ (x) (eq? (car x) 0)) exported-variables))
  (make-asset
   (for/hash ([id (sequence-map car (in-list (cdr phase0)))])
     (values id (dynamic-require module-path id)))))

(define (racket-modules make-module-path [make-asset make-asset-from-provides])
  (λ (key recurse)
    (let ([module-path (make-module-path key)])
      (and module-path
           (make-pod/fenced
            key
            (make-fence-thunk #:capture? #t
                              (λ () (dynamic-rerequire module-path #:verbosity 'none))
                              (λ (a b) (null? b)))
            (λ () (make-asset module-path)))))))
