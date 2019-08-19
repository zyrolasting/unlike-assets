#lang racket/base

(require
  rackunit
  racket/list
  racket/class
  racket/function
  net/url
  "assets.rkt")

(test-case
  "Add vertices to graph"
  (define G (new assets%))
  (send G add! 'a 'x)
  (send G add! 'b 'y)
  (check-true (send G has? 'a))
  (check-true (send G has? 'b))
  (check-equal? (send G lookup/latest 'a) 'x)
  (check-equal? (send G lookup/latest 'b) 'y)
  (check-equal? (send G lookup/latest '?) #f)
  (check-equal?
    (send G ->hash)
    `#hash((a . x) (b . y))))

(test-case
  "Model responsibility with directed edges"
  (define G (new assets%))
  (send G add! 'a void)
  (send G add! 'b #f)
  (send G add! 'c void)
  (send G make-responsible! 'b 'a identity)
  (send G make-responsible! 'c 'b identity)

  (check-equal? (send G get-dependents 'a) null)
  (check-equal? (send G get-dependents 'b) '(a))
  (check-equal? (send G get-dependents 'c) '(b))

  (check-equal? (send G get-dependencies 'a) '(b))
  (check-equal? (send G get-dependencies 'b) '(c))
  (check-equal? (send G get-dependencies 'c) null)

  (check-equal? (send G get-first-advanceable) 'c)
  (send G progress! 'c #f)
  (check-equal? (send G get-first-advanceable) 'a)
  (send G progress! 'a #f)
  (check-equal? (send G get-first-advanceable) #f))

(test-exn
  "Disallow cycles in graph"
  exn:fail?
  (λ _
     (define G (new assets%))
     (send G add! 'a #f)
     (send G add! 'b #f)
     (send G make-responsible! 'a 'b)
     (send G make-responsible! 'b 'a)))

(test-case
  "Updating history"
  (define G (new assets%))
  (send G add! 'a 1)
  (send G progress! 'a 2)
  (check-equal? (send G lookup/latest 'a) 2)
  (check-equal? (send G lookup/history 'a) '(2 1))
  (send G update! 'a 3)
  (check-equal? (send G lookup/latest 'a) 3)
  (check-equal? (send G lookup/history 'a) '(3)))


(test-case
  "Regress vertex and dependents"
  (define G (new assets%))
  (define (stack5 . syms)
    (for ([v syms])
      (send G add! v 1)
      (for ([i (range 2 6)])
        (send G progress! v i))))
  (stack5 'a 'b 'c 'd 'e)

  #|
  e --> b --> a --> d
        +---> c --> f
  |#
  (send G make-responsible! 'e 'b (λ _ 3))
  (send G make-responsible! 'b 'a (λ _ 2))
  (send G make-responsible! 'a 'd (λ _ 4))
  (send G make-responsible! 'b 'c (λ _ 5))
  (send G make-responsible! 'c 'f (λ _ 99)) ; not in history

  (send G regress! 'e 'b #f)
  (send G regress! 'b 'a #f)
  (send G regress! 'b 'c #f)
  (send G regress! 'c 'f #f)
  (send G regress! 'a 'd #f)

  (check-equal? (send G lookup/history 'a) '(2 1))
  (check-equal? (send G lookup/history 'b) '(3 2 1))
  (check-equal? (send G lookup/history 'c) '(5 4 3 2 1))
  (check-equal? (send G lookup/history 'd) '(4 3 2 1))
  (check-equal? (send G lookup/history 'e) '(5 4 3 2 1))
  (check-equal? (send G lookup/history 'f) '(#f)))
