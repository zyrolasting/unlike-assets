#lang racket/base

(require racket/contract
         "base.rkt"
         "asset.rkt"
         "pod.rkt")

(provide (rename-out [procure/weak Pw] [procure Ps])
         define-procured
         (contract-out
          [in-assets (->* () ((-> asset? any/c)) sequence?)]
          [u/a (->* () #:rest (listof route/c) void?)]
          [current-resolver (parameter/c resolver?)]
          [procure/weak (->* (string?) (#:make-alias (or/c #f (-> asset? string?))) (-> asset?))]
          [procure (->* (string?) (#:make-alias (or/c #f (-> asset? string?))) #:rest (listof symbol?) any/c)]))

(require racket/format
         racket/sequence
         racket/set
         racket/string)

(define (in-assets [keep? (λ _ #t)])
  (in-found (current-resolver) asset? keep?))

(define (u/a . ps)
  (current-resolver (apply make-resolver #:known ((current-resolver)) ps)))

(define current-resolver (make-parameter (make-resolver)))

(define (procure #:make-alias [make-alias #f] key . syms)
  (define r (current-resolver))
  (define a
    (if make-alias
        (r key asset? make-alias)
        (r key asset?)))

  (if (null? syms)
      a
      (if (= (length syms) 1)
          (a (car syms))
          (apply values (map a syms)))))

(define-syntax-rule (define-procured key ids ...)
  (define-values (ids ...) (procure key 'ids ...)))

(define (procure/weak key #:make-alias [make-alias #f])
  (define r (current-resolver))
  (define p (r key))
  (p)
  (λ () (if make-alias
            (r key asset? make-alias)
            (r key asset?))))

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
