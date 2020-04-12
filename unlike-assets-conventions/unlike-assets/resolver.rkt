#lang racket/base

(require racket/contract)
(provide (all-from-out "private/system.rkt"
                       "private/assets.rkt")
         (rename-out [procure/weak Pw]
                     [procure/strong Ps])
         (contract-out
          [current-key->live-build (parameter/c (-> string? u/a-build-system? live-build?))]
          [current-u/a-build-system (parameter/c u/a-build-system?)]
          [procure/weak (-> string? stateful-cell?)]
          [procure/strong (->* (string?) #:rest (listof symbol?) any/c)]))

(require kinda-ferpy)

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
