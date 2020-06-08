#lang racket/base

;; This module is responsible for cycle detection and dependency
;; tracking.

(provide get-dependents
         dependent)

(require "../exn.rkt")

(define (get-dependents scope)
  (hash-ref (get-dependents-lookup) scope null))

(define-syntax-rule (dependent scope key body ...)
  (enter-dependent-section scope key (λ () body ...)))

(define mark-key (string->uninterned-symbol "unlike-assets:dependents"))

(define (get-dependents-lookup)
  (or (continuation-mark-set-first
       (current-continuation-marks)
       mark-key)
      #hash()))

(define (in-cycle? key dependents)
  (and (list? dependents)
       (member key dependents)
       #t))

(define (add-dependent scope key)
  (define lookup (get-dependents-lookup))
  (define dependents (cons key (get-dependents scope)))
  (hash-set lookup scope dependents))

(define (enter-dependent-section scope key proc)
  (let ([dependents (get-dependents scope)])
    (when (in-cycle? key dependents)
      (raise-cycle-error scope key dependents))
    (with-continuation-mark mark-key (add-dependent scope key)
      (proc))))

(module+ test
  (require rackunit)
  (define-syntax-rule (expect-cycle expected-scope expected-dependency-key expected-dependencies body ...)
    (check-exn
     (λ (e) (and (exn:fail:unlike-assets:cycle? e)
                 (eq? (exn:fail:unlike-assets:cycle-scope e) expected-scope)
                 (equal? (exn:fail:unlike-assets:cycle-dependency e) expected-dependency-key)
                 (equal? (exn:fail:unlike-assets:cycle-dependents e) expected-dependencies)))
     (λ () body ...)))

  (define-syntax-rule (expect-no-cycle body ...)
    (check-not-exn (λ () body ...)))

  (test-case "Can detect cycles"
    (expect-cycle
     0 "a" '("a")
     (dependent 0 "a" (dependent 0 "a" (void))))

    (expect-cycle
     0 "a" '("b" "a")
     (dependent 0 "a" (dependent 0 "b" (dependent 0 "a" (void)))))

    (expect-no-cycle (dependent 0 "a" (dependent 0 "b" (dependent 0 "c" (void)))))
    (expect-no-cycle (dependent 1 "a" (dependent 0 "a" (void)))))

  (test-case "Can trace dependents"
    (check-equal?
     (dependent 0 "a" (dependent 0 "b" (dependent 0 "c" (get-dependents 0))))
     '("c" "b" "a"))))
