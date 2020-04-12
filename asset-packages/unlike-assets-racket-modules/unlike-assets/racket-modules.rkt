#lang racket/base

(require racket/string
         racket/rerequire
         unlike-assets/reactive)

(define (key->maybe-build key recurse)
  (and (string-suffix? key ".rkt")
       (start-live-build! key
                          #:sample! (λ () (dynamic-rerequire key))
                          #:build! (λ _ (dynamic-require key 'make-asset))
                          #:suppress? (λ (a b) (null? b)))))
