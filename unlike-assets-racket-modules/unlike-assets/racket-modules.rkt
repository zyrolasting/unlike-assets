#lang racket/base

(require racket/rerequire
         unlike-assets/resolver)

(define (racket-modules make-module-path)
  (λ (key recurse)
    (let ([module-path (make-module-path key)])
      (and module-path
           (start-live-build! key
                              #:sample! (λ () (dynamic-rerequire key #:verbosity 'none))
                              #:build! (λ _ (dynamic-require key 'make-asset))
                              #:suppress? (λ (a b) (null? b)))))))
