#lang racket/base

(require (only-in racket/function curry identity negate)
         racket/undefined
         kinda-ferpy)

(provide start-live-build!
         make-u/a-build-system
         live-build?
         u/a-build-system?)

(define (of-name str)
  (let ([s (string->uninterned-symbol str)])
    (values
     (λ (p) (procedure-rename p s))
     (λ (p) (eq? (object-name p) s)))))

(define-values (make-live-build-proc live-build?) (of-name "live-build"))
(define-values (make-u/a-build-system-proc u/a-build-system?) (of-name "u/a-build-system"))

(define (<log level event target)
  (log-message (current-logger)
               level
               'unlike-assets
               (format "~a: ~a" event target)
               target))

(define (apply-until v [stop? (negate procedure?)])
  (if (stop? v)
      v
      (apply-until (v) stop?)))

(define (start-live-build! #:sample!   sample!
                           #:suppress? [suppress? eq?]
                           #:build!    build!
                           key)
  (define initial-value (sample!))
  (define %signal (% initial-value))
  (define %producer
    (make-stateful-cell/async
     #:dependencies (list %signal)
     (λ ()
       (<log 'info "UPDATE" key)
       (build! (%signal)))))
  (make-live-build-proc
   (λ ([stop? stateful-cell?])
     (<log 'debug "SIGNAL" key)
     (define next (sample!))
     (unless (suppress? (%signal) next)
       (%signal next))
     (apply-until %producer stop?))))


(define (make-u/a-build-system key->live-build)
  (define known (make-hash))

  (define u/a
    (make-u/a-build-system-proc
     (case-lambda
       [() known]
       [(key) (live-build-ref key)]
       [(key stop?) ((live-build-ref key) stop?)])))

  (define (live-build-ref key)
    (unless (hash-has-key? known key)
      (hash-set! known key (key->live-build key u/a)))
    (hash-ref known key))

  u/a)
