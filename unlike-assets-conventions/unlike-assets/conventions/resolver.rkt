#lang racket/base

(require racket/contract
         kinda-ferpy
         unlike-assets/core)

(provide (rename-out [procure/weak Pw]
                     [procure/strong Ps]
                     [procure/strong/with-contract Ps/c])
         (contract-out
          [make-key->live-build/sequence
           (->* ()
                #:rest (listof (-> string? u/a-build-system? (or/c #f live-build?)))
                (-> string? u/a-build-system? (or/c #f live-build?)))]
          [current-key->live-build (parameter/c (-> string? u/a-build-system? live-build?))]
          [current-u/a-build-system (parameter/c u/a-build-system?)]
          [procure/weak (-> string? stateful-cell?)]
          [procure/strong (->* (string?) #:rest (listof symbol?) any/c)]
          [procure/strong/with-contract (-> string? contract? asset?)]))

(define current-key->live-build
  (make-parameter (λ _ (error "current-key->live-build is not defined"))))

(define current-u/a-build-system
  (make-parameter (make-u/a-build-system
                   (λ (k s) ((current-key->live-build) k s)))))

(define (procure/weak key)
  ((current-u/a-build-system) stateful-cell?))

(define (procure/strong key . syms)
  (apply (make-u/a-procure-procedure (current-u/a-build-system))
         key syms))

(define (make-key->live-build/sequence . maybe-makers)
  (λ (key recurse)
    (ormap (λ (p) (p key recurse))
           maybe-makers)))

(define (procure/strong/with-contract key c)
  (with-contract procure/strong/with-contract #:result c (procure/strong key)))

(module+ test
  (require rackunit)
  (define Ps procure/strong)
  (define Pw procure/weak)
  (define Ps/c procure/strong/with-contract)

  (define (key->asset-build key sys)
    (start-live-build! key
                       #:sample! (λ _ #f)
                       #:build! (λ _ (asset [key key]
                                            [up (string-upcase key)]))
                       #:suppress? equal?))

  (test-case "A shared build system is available"
    (test-exn "The shared system requires you to map keys to live builds"
              exn:fail?
              (current-key->live-build))

    (parameterize ([current-key->live-build key->asset-build])
      (check-equal? (Ps "a" 'up) "A")))

  (test-case "You can request assets that match a contract"
    (parameterize ([current-key->live-build key->asset-build])
      (check-exn exn:fail:contract?
                 (λ () (Ps/c "a" (asset/c [up number?]))))
      (check-not-exn (λ () (Ps/c "a" (asset/c [up string?])))))))
