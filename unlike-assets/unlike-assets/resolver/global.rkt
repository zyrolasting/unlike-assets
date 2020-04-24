#lang racket/base

(require racket/contract
         "base.rkt"
         "asset.rkt"
         "pod.rkt")

(define procure/weak/c (->* (string?) (-> asset?)))
(define procure/c (->* (string?) #:rest (listof symbol?) any))

(provide define-procured
         (contract-out
          [in-assets (->* () ((-> asset? (non-empty-listof string?) any/c)) sequence?)]
          [u/a (->* () #:rest (listof route/c) void?)]
          [current-resolver (parameter/c resolver?)]
          [procure/weak procure/weak/c]
          [procure procure/c]
          [rename procure/weak Pw procure/weak/c]
          [rename procure Ps procure/c]))

(require racket/format
         racket/sequence
         racket/set
         racket/string)

(define (in-assets [keep? (λ _ #t)])
  (in-found (current-resolver) asset? keep?))

(define (u/a . ps)
  (current-resolver (apply make-resolver #:known ((current-resolver)) ps)))

(define current-resolver (make-parameter (make-resolver)))

(define (procure key . syms)
  (let ([a ((current-resolver) key asset?)])
    (if (null? syms)
        a
        (if (= (length syms) 1)
            (a (car syms))
            (apply values (map a syms))))))

(define-syntax-rule (define-procured key ids ...)
  (define-values (ids ...) (procure key 'ids ...)))

(define (procure/weak key)
  (define r (current-resolver))
  (define p (r key))
  (p)
  (λ () (procure key)))

(module+ test
  (require rackunit)
  (define P procure)
  (define Pw procure/weak)

  (define-syntax-rule (with-u/a (ps ...) body ...)
    (parameterize ([current-resolver (make-resolver ps ...)])
      body ...))

  (define (uppers key sys)
    (pod key
         (asset [key key]
                [up (string-upcase key)])))

  (test-case "A shared resolver is available"
    (test-exn "By default, it requires you to map keys to pods"
              exn:fail?
              (λ () ((current-resolver) "")))
    (with-u/a [uppers]
      (check-equal? (P "a" 'up) "A"))))
