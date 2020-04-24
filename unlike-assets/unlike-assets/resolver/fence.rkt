#lang racket/base

(require racket/bool
         racket/contract
         idiocket/function
         racket/undefined)

(provide
 fence
 (contract-out
  [equal?/raised
   (-> any/c any/c boolean? boolean? boolean?)]
  [make-fence-thunk
   (->* ((-> any/c))
        ((or/c (-> any/c any/c boolean? boolean? any/c)
               (-> any/c any/c any/c))
         #:capture? any/c)
        (-> boolean?))]))

(define (equal?/raised pv nv pr nr)
  (cond [(and pr nr) ; both raised
         (if (and (exn? pv) (exn? nv))
             (or (eq? pv nv)
                 (equal? (exn-message pv) (exn-message nv)))
             (equal? pv nv))]
        [(xor pr nr) ; one raised
         #f]
        [else ; neither raised
         (equal? pv nv)]))

(define-syntax-rule (fence body ...)
  (make-fence-thunk (λ () body ...)
                    equal?/raised
                    #:capture? #t))

(define (make-fence-thunk sample
                          #:capture? [capture? #f]
                          [same? (if capture? equal?/raised equal?)])
  (define (!)
    (with-handlers ([(const capture?) (λ (v) (values #t v))])
      (values #f (sample))))

  (define-values (prev-raised prev-val) (!))
  (define first-call? #t)

  (define ?
    (negate
     (if (= (procedure-arity same?) 4)
         same?
         (λ (pv nv pr nr) (same? pv nv)))))

  (define (update! next-raised next-val)
    (begin0 (and (? prev-val next-val prev-raised next-raised)
                 #t)
      (set! prev-raised next-raised)
      (set! prev-val next-val)))

  (λ ()
    (if first-call?
        (begin0 first-call?
          (set! first-call? #f))
        (call-with-values ! update!))))

(module+ test
  (require rackunit
           racket/generator
           racket/sequence)

  (define (make-sample seq) (generator () (sequence-for-each yield seq)))

  (test-case "equal?/raised is like equal?, but adds casework for if operands were raised"
    (test-case "If one value was raised and the other wasn't, they are not the same."
                (check-false (equal?/raised 1 1 #t #f))
                (check-false (equal?/raised 1 1 #f #t)))
    (test-case "If both values (or neither value) were raised, they are compared normally."
                (check-true (equal?/raised 1 1 #t #t))
                (check-true (equal?/raised 1 1 #f #f)))
    (test-case "If both raised values are exceptions, they are compared by eq?, then error message"
      (define a (exn "a" (current-continuation-marks)))
      (define b (exn "b" (current-continuation-marks)))
      (check-false (equal?/raised a b #t #t))
      (check-true (equal?/raised a a #t #t))
      (check-true (equal?/raised b b #t #t))))

  (test-case "A fence thunk can be applied repeatedly to detect change"
    (define sample (make-sample '(1 1 2 1)))

    ; Captures 1 as first value
    (define s (make-fence-thunk sample))

    ; The first call is unconditionally #t,
    ; since the change is from no value to some value.
    (check-true (s))

    ; #f, no change. Second value is also 1.
    (check-false (s))

    ; #t, because 2 followed 1.
    (check-true (s))

    ; #t, because 1 followed 2.
    (check-true (s)))

  (test-case "You can write your own comparisons"
    (define seq (list 1 (void) (void) 2 (void)))
    (define sample (make-sample seq))
    (define s (make-fence-thunk sample (λ (a b) (void? b))))
    (check-equal? (for/list ([_ seq]) (s))
                  '(#t #f #f #t #f)))

  (test-case "A fence thunk can incorporate raised values in comparisons"
    ; Interpret this list as flags that control whether sample should raise.
    (define seq (list #t #f #t #f #t #f #f #f #f))
    (define end (sub1 (length seq)))
    (define b (box 0))
    (define (sample)
      (define i (unbox b))
      (set-box! b (min end (add1 i)))
      (if (list-ref seq i)
          (raise i)
          i))

    ; Meaning: There was no change if values were both raised,
    ; or neither of them were raised.
    (define (same? prev-v next-v prev-raised next-raised)
      ; Verify that the original list agrees with the raised flags.
      (when prev-raised (check-true (list-ref seq prev-v)))
      (when next-raised (check-true (list-ref seq next-v)))
      (eq? prev-raised next-raised))

    (check-not-exn
     (λ ()
       (define s (make-fence-thunk sample same? #:capture? #t))
       (check-equal? (for/list ([_ seq]) (s))
                     '(#t #t #t #t #t #t #f #f #f))))))
