#lang racket/base

(require racket/contract
         racket/rerequire
         racket/path
         unlike-assets/resolver)

(define module-path/c
  (or/c module-path? resolved-module-path? module-path-index?))

(provide
 module-path/c
 (contract-out
  [racket-modules (->* ((or/c path-for-some-system?
                              path-string?
                              (-> string? module-path/c)))
                       #:rest (listof any/c)
                       procedure?)]))

(define (normalize-make-module-path v)
  (if (or (path-for-some-system? v)
          (path-string? v))
      (if (directory-exists? v)
          (λ (key)
            (with-handlers ([exn:fail? (λ _ #f)])
              (define maybe-racket-module-path (build-path v key))
              (and ((flat-contract-predicate module-path/c) maybe-racket-module-path)
                   (equal? (path-get-extension maybe-racket-module-path) #".rkt")
                   maybe-racket-module-path)))
          (raise-argument-error 'racket-modules
                                "A path to an existing directory"
                                v))
      v))


(define (racket-modules make-module-path . user-data)
  (define normalized (normalize-make-module-path make-module-path))
  (λ (key recurse)
    (let ([module-path (normalized key)])
      (and module-path
           (start-live-build! key
                              #:sample! (λ () (dynamic-rerequire key #:verbosity 'none))
                              #:build! (λ _ (apply (dynamic-require key 'make-asset) user-data))
                              #:suppress? (λ (a b) (null? b)))))))
