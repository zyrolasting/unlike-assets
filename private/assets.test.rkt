#lang racket/base

(require
  rackunit
  net/url
  "./assets.rkt")

(provide w/deps)

(define (w/deps src . rest) (unlike-asset #f rest src))

(parameterize ([current-clarify-unlike-proc string-upcase])
  (test-case "(dependencies-met?)"
    (define responsible (w/deps "a"))
    (define dependent (w/deps "b" "a"))
    (define irresponsible (w/deps "c" "b" "a"))
    (define available (make-hash `(("A" . ,responsible)
                                   ("B" . ,dependent)
                                   ("C" . ,irresponsible))))

    (check-true (dependencies-met? irresponsible available))
    (hash-remove! available "A")
    (check-false (dependencies-met? irresponsible available)))

  (test-case "((unlike-asset)): no dependencies"
    (define initial (unlike-asset (λ _ (dependent (+ 1 2) '())) null "src"))
    (define advanced (initial #hash()))
    (check-equal? (dependent-val advanced) 3)
    (check-true (unlike-asset? advanced))
    (check-equal? (unlike-asset-requested-as advanced)
                  (unlike-asset-requested-as initial))
    (check-true (fulfilled? advanced)))

  (test-case "((unlike-asset)): with dependencies"
    (define (dependent-sum a avail)
      (dependent (apply + (dependent-val (hash-ref avail "A"))) '()))

    (define responsible (unlike-asset '(1 2) null "A"))
    (define irresponsible (unlike-asset dependent-sum '("A") "B"))
    (define available (make-hash (list (cons "A" responsible))))
    (check-equal? (dependent-val (irresponsible available)) 3)))
