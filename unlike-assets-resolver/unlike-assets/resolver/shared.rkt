#lang racket/base

(require racket/contract
         racket/sequence
         racket/set
         kinda-ferpy
         "model.rkt"
         "assets.rkt")

(provide (rename-out [procure/weak Pw]
                     [procure/strong Ps]
                     [procure/strong/with-contract Ps/c])
         (contract-out
          [in-assets (->* () (u/a-build-system? #:keep? (or/c flat-contract? (-> asset? any/c))) sequence?)]
          [u/a (->* () #:rest (listof ->live-build/c) void?)]
          [current-key->live-build (parameter/c ->live-build/c)]
          [current-u/a-build-system (parameter/c u/a-build-system?)]
          [procure/weak (-> string? stateful-cell?)]
          [procure/strong (->* (string?) #:rest (listof symbol?) any/c)]
          [procure/strong/with-contract (-> string? contract? asset?)]
          [asset/with-read/c contract?]
          [asset/with-write/c contract?]))

(define asset/with-write/c
  (asset/c [write (-> output-port? (or/c void? exact-nonnegative-integer?))]))

(define asset/with-read/c
  (asset/c [read (-> input-port? any/c)]))

(define (in-assets [sys (current-u/a-build-system)] #:keep? [keep? (λ _ #t)])
  (sequence-filter (if (flat-contract? keep?) (flat-contract-predicate keep?) keep?)
                   (sequence-map (λ (living-build)
                                   (living-build asset?))
                                 (in-set (apply seteq (hash-values (sys)))))))

(define (no-impl . _)
  (error "current-key->live-build is not defined"))

(define current-key->live-build
  (make-parameter no-impl))

(define current-u/a-build-system
  (make-parameter (make-u/a-build-system
                   (λ (k s) ((current-key->live-build) k s)))))

(define (extend! p)
  (define current (current-key->live-build))
  (current-key->live-build
   (if (eq? current no-impl)
       p
       (λ (k r)
         (or (current k r)
             (p k r))))))

(define (u/a . ps)
  (void (map extend! ps)))

(define (procure/weak key)
  ((current-u/a-build-system) stateful-cell?))

(define (procure/strong key . syms)
  (apply (make-u/a-procure-procedure (current-u/a-build-system))
         key syms))

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
