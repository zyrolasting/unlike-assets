#lang racket/base

(require racket/contract
         kinda-ferpy
         "base.rkt"
         "asset.rkt"
         "pod.rkt")

(provide (rename-out [procure/weak Pw] [procure Ps])
         define-procured
         (contract-out
          [in-assets (->* () ((-> asset? any/c)) sequence?)]
          [u/a (->* () #:rest (listof route/c) void?)]
          [current-resolver (parameter/c resolver?)]
          [procure/weak (-> string? stateful-cell?)]
          [procure (->* (string?) #:rest (listof symbol?) any/c)]))

(require racket/format
         racket/sequence
         racket/set
         racket/string)

(define (in-assets [keep? (λ _ #t)])
  (in-found (current-resolver) asset? keep?))

(define (u/a . ps)
  (current-resolver (apply make-resolver #:known ((current-resolver)) ps)))

(define current-resolver (make-parameter (make-resolver)))

#;  (log-message (current-logger)
               'debug
               'unlike-assets
               (format "(procure ~a~a)"
                       key
                       (if (null? syms)
                           ""
                           (string-join (map ~a syms) " ")))
               (current-continuation-marks))

(define (procure key . syms)
  (let ([la ((current-resolver) key asset?)])
    (if (null? syms)
        la
        (if (= (length syms) 1)
            (la (car syms))
            (apply values (map la syms))))))

(define-syntax-rule (define-procured key ids ...)
  (define-values (ids ...) (procure key 'ids ...)))

(define (procure/weak key)
  (define p ((current-resolver) key))
  (p)
  p)

(module+ test
  (require rackunit)
  (define P procure)
  (define Pw procure/weak)

  (define-syntax-rule (with-u/a (ps ...) body ...)
    (parameterize ([current-resolver (make-resolver ((current-resolver)) ps ...)])
      body ...))

  (define (uppers key sys)
    (pod key
         #f
         (asset [key key]
                [up (string-upcase key)])))

  (test-case "A shared resolver is available"
    (test-exn "By default, it requires you to map keys to pods"
              exn:fail?
              (λ () ((current-resolver) "")))
    (with-u/a [uppers]
      (check-equal? (P "a" 'up) "A"))))
