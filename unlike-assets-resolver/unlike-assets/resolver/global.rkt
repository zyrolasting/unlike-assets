#lang racket/base

(require racket/contract
         kinda-ferpy
         "base.rkt"
         "asset.rkt"
         "pod.rkt")

(provide (rename-out [procure/weak Pw] [procure Ps])
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
  (current-resolver (apply make-resolver ((current-resolver)) ps)))

(define current-resolver (make-parameter (make-resolver)))

(define (procure key . syms)
  (log-message (current-logger)
               'debug
               'unlike-assets
               (format "(procure ~a~a)"
                       key
                       (if (null? syms)
                           ""
                           (string-join (map ~a syms) " ")))
               (current-continuation-marks))
  (let ([la ((current-resolver) key asset?)])
    (if (null? syms)
        la
        (if (= (length syms) 1)
            (la (car syms))
            (apply values (map la syms))))))

(define (procure/weak key)
  ((current-resolver) key))

(module+ test
  (require rackunit)
  (define P procure)
  (define Pw procure/weak)

  (define-syntax-rule (with-u/a (ps ...) body ...)
    (parameterize ([current-resolver (make-resolver ((current-resolver)) ps ...)])
      body ...))

  (define (uppers key sys)
    (pod (asset [key key]
                [up (string-upcase key)])))

  (test-case "A shared resolver is available"
    (test-exn "By default, it requires you to map keys to pods"
              exn:fail?
              (λ () ((current-resolver) "")))
    (with-u/a [uppers]
      (check-equal? (P "a" 'up) "A"))))
