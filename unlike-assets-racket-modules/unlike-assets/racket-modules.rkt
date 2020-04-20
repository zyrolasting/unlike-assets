#lang racket/base

(require racket/contract
         racket/function
         racket/rerequire
         racket/path
         racket/sequence
         unlike-assets/resolver)

(define module-path/c
  (or/c path-for-some-system?
        module-path?
        resolved-module-path?
        module-path-index?))

(provide
 module-path/c
 (contract-out
  [make-asset-from-provides (-> module-path/c asset?)]
  [racket-modules (->* ((or/c path-for-some-system?
                              path-string?
                              (-> string? (or/c path-string? module-path/c #f))))
                       ((-> module-path/c asset?))
                       procedure?)]))

(define (normalize-make-module-path v)
  (if (or (path-for-some-system? v)
          (path-string? v))
      (if (directory-exists? v)
          (λ (key)
            (with-handlers ([exn:fail? (λ _ #f)])
              (define maybe-racket-module-path (simplify-path (build-path v key)))
              (and ((flat-contract-predicate module-path/c) maybe-racket-module-path)
                   (equal? (path-get-extension maybe-racket-module-path) #".rkt")
                   maybe-racket-module-path)))
          (raise-argument-error 'racket-modules
                                "A path to an existing directory"
                                v))
      (λ (key)
        (define modpath (v key))
        (if (string? modpath)
            (string->path modpath)
            modpath))))

(define (make-asset-from-provides module-path)
  (dynamic-require module-path (void))
  (define-values (exported-variables _) (module->exports module-path))
  (define phase0 (findf (λ (x) (eq? (car x) 0)) exported-variables))
  (make-asset
   (for/hash ([id (sequence-map car (in-list (cdr phase0)))])
     (values id (dynamic-require module-path id)))))

(define (racket-modules make-module-path [make-asset make-asset-from-provides])
  (define normalized (normalize-make-module-path make-module-path))
  (λ (key recurse)
    (let ([module-path (normalized key)])
      (and module-path
           (pod [(λ (a b) (null? b)) _ <- (dynamic-rerequire module-path #:verbosity 'none)]
                (make-asset module-path))))))
