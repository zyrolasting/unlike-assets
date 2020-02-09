#lang racket/base

(require kinda-ferpy)

(provide start-live-build
         make-u/a-build-system
         live-build?
         u/a-build-system?
         get-live-build
         maybe-build-u/a-cell!
         wait-for-u/a-cell!
         procure-u/a!)

(define (of-name str)
  (let ([s (string->uninterned-symbol str)])
    (values
     (λ (p) (procedure-rename p s))
     (λ (p) (eq? (object-name p) s)))))

(define-values (make-live-build-proc live-build?)
  (of-name "live-build"))
(define-values (make-u/a-build-system-proc u/a-build-system?)
  (of-name "u/a-build-system"))

(define (start-live-build sample-change respond [suppress? eq?])
  (define initial-value (sample-change))
  (define %signal (% initial-value))
  (define %producer
    (make-stateful-cell/async
     #:dependencies (list %signal)
     (λ () (respond (%signal)))))
  (make-live-build-proc
   (λ ()
     (define next-value (sample-change))
     (unless (suppress? (%signal) next-value)
       (%signal next-value))
     %producer)))

(define (make-u/a-build-system key->live-build)
  (define known (make-hash))
  (define (live-build-ref key)
    (unless (hash-has-key? known key)
      (hash-set! known key (key->live-build key live-build-ref)))
    (hash-ref known key))
  (make-u/a-build-system-proc
   (case-lambda
     [() known]
     [(key) (live-build-ref key)])))

(define (get-live-build sys u)
  (sys u))

(define (maybe-build-u/a-cell! sys u)
  ((get-live-build sys u)))

(define (wait-for-u/a-cell! sys u)
  ((maybe-build-u/a-cell! sys u)))

(define (procure-u/a! sys u)
  ((wait-for-u/a-cell! sys u)))

(module+ test
  (require rackunit)

  (test-pred "Can recognize live build procedures"
             live-build?
             (start-live-build void void))

  (test-pred "Can recognize build systems"
             u/a-build-system?
             (make-u/a-build-system void))

  (test-case "Live builds start immediately"
    (define ch (make-channel))
    (define build (start-live-build void (λ _ (channel-put ch 'what))))
    (define deadline (alarm-evt (+ (current-inexact-milliseconds) 500)))
    (check-eq? 'what (sync deadline ch)))

  (test-case "Build systems store encountered live builds"
    (define sys (make-u/a-build-system
                 (λ (key _) (start-live-build (λ _ key)
                                              (λ _ key)
                                              equal?))))
    (check-equal? (make-hash) (sys))
    (sys "a")
    (check-true (hash-has-key? (sys) "a")
                (live-build? (hash-ref (sys) "a")))))
