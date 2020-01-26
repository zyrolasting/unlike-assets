#lang racket/base

(provide stateful-cell v^)

;; Based on https://github.com/MaiaVictor/PureState
;; See tests for usage details and a design limitation.
;;
;; Dependencies are detected implicitly.

(define captured-deps '())
(define capture? (make-parameter #f))
(define not-set (gensym))

(define (normalize compute)
  (if (procedure? compute)
      compute
      (λ _ compute)))

(struct node (dependencies dependents compute value)
  #:mutable #:property prop:procedure
  (λ (self [new-compute not-set])
     (when (capture?)
       (set! captured-deps (cons self captured-deps)))
     (unless (eq? new-compute not-set)
       (set-node-compute! self (normalize new-compute))
       (refresh! self))
     (node-value self)))

(define (refresh! n)
  (set-node-value! n
                   (apply (node-compute n)
                          (map node-value (node-dependencies n))))
  (for ([dependent (node-dependents n)])
    (refresh! dependent)))

(define (stateful-cell compute)
  (define n (node '() '() (normalize compute) (void)))
  (set! captured-deps '())
  (parameterize ([capture? #t])
    ((node-compute n)))
  (set-node-dependencies! n captured-deps)
  (for ([captured-dependency captured-deps])
    (set-node-dependents! captured-dependency
                          (cons n (node-dependents captured-dependency))))
  (refresh! n)
  n)

(define v^ stateful-cell)

(module+ test
  (require rackunit)

  (define (with-call-counter proc)
    (let ([num-calls 0])
      (values (λ A
                (set! num-calls (add1 num-calls))
                (apply proc A))
              (λ _ num-calls))))

  (test-case "State graph procedures represent data or other procedures trivially"
    (let ([x 10])
      (check-equal? ((v^ x)) x)
      (check-equal? ((v^ (λ _ x))) x)))

  (test-case "Racket values can form dependency relationships"
    (let* ([a (v^ 1)]
           [b (v^ 1)]
           [c (v^ (λ _ (+ (a) (b))))])
      (check-equal? (c) 2)
      (a 2)
      (check-equal? (c) 3)))

  (test-case "Dependencies are not recomputed unless necessary"
    (define-values (a a-count) (with-call-counter (λ _ 1)))

    ; Sanity check the counter
    (test-equal? "a was not yet called" (a-count) 0)

    (define stateful-a (v^ a))
    (test-equal? "a is called twice; Once for discovery and once for initialization."
                 (a-count) 2)

    (define-values (b b-count) (with-call-counter (λ _ 1)))
    (define stateful-b (v^ b))
    (define-values (c c-count) (with-call-counter (λ _ (+ (stateful-a) (stateful-b)))))
    (define stateful-c (v^ c))

    (define (check-counts expectation
                          before
                          expected-a-count
                          expected-b-count
                          expected-c-count)
      (before)
      (test-case expectation
        (test-equal? "a count matches" (a-count) expected-a-count)
        (test-equal? "b count matches" (b-count) expected-b-count)
        (test-equal? "c count matches" (c-count) expected-c-count)))

    (check-counts "Nothing was called an additional time because nothing changed."
                  stateful-c
                  2 2 2)
    (check-counts "Updating one dependency updates dependents"
                  (λ _
                    (stateful-a a)
                    (stateful-c))
                  3 2 3))

  (test-case "Dependencies are detected only when called during discovery."
    ; Ported example from https://github.com/MaiaVictor/PureState/issues/4
    (define x (v^ #t))
    (define y (v^ 2))

    ; y won't be marked as a dependency of z
    ; because (y) does not evaluate.
    (define z (v^ (λ _ (if (x) 1 (y)))))
    (x #f)

    (check-equal? (z) 2)
    (y 3)

    ; This is incorrect, but consistent.
    ; This is why programmers need to change their mode of
    ; writing.
    (check-equal? (z) 2)))
