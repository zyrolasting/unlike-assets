#lang racket/base

(require racket/contract
         idiocket/function
         racket/undefined
         kinda-ferpy)

(provide start-live-build!
         make-u/a-build-system
         live-build?
         u/a-build-system?
         ->live-build/c)

(define-values (make-live-build-proc live-build?) (of-name "live-build"))
(define-values (make-u/a-build-system-proc u/a-build-system?) (of-name "u/a-build-system"))

(define ->live-build/c
  (-> string? u/a-build-system? (or/c #f live-build?)))

(define (<log event target)
  (log-message (current-logger)
               'debug
               'unlike-assets
               (format "~a: ~a" event target)
               target))

(define (start-live-build! key #:sample! sample! #:suppress? [suppress? eq?] #:build! build!)
  (define initial-value (sample!))
  (define %signal (% initial-value))
  (define %producer
    (make-stateful-cell/async
     #:dependencies (list %signal)
     (λ ()
       (<log "UPDATE" key)
       (build! (%signal)))))
  (make-live-build-proc
   (λ ([stop? stateful-cell?])
     (<log "SIGNAL" key)
     (define next (sample!))
     (unless (suppress? (%signal) next)
       (%signal next))
     (apply-until %producer stop?))))

(define (make-u/a-build-system key->live-build [known (make-hash)])
  (define u/a
    (make-u/a-build-system-proc
     (case-lambda
       [() known]
       [(key) (live-build-ref key)]
       [(key stop?) ((u/a key) stop?)]
       [(key stop? make-alias)
        (define val (u/a key stop?))
        (hash-set! known (make-alias key val) (u/a key))
        val])))
  (define (live-build-ref key)
    (unless (hash-has-key? known key)
      (hash-set! known key (key->live-build key u/a)))
    (hash-ref known key))
  u/a)

(module+ test
  (require rackunit)

  (define (key->key-build key sys)
    (start-live-build! key
                       #:sample! (λ _ #f)
                       #:build! (λ _ (string-upcase key))
                       #:suppress? equal?))

  (test-pred "Can recognize live build procedures"
             live-build?
             (start-live-build! "" #:sample! void #:build! void))

  (test-pred "Can recognize build systems"
             u/a-build-system?
             (make-u/a-build-system void))

  (test-case "Live builds start immediately"
    (define ch (make-channel))
    (define build (start-live-build! ""
                                     #:sample! void
                                     #:build! (λ _ (channel-put ch 'what))))
    (define deadline (alarm-evt (+ (current-inexact-milliseconds) 500)))
    (check-eq? 'what (sync deadline ch)))

  (test-case "Build systems store encountered live builds"
    (define sys (make-u/a-build-system
                 (λ (key _) (start-live-build! ""
                                               #:sample! (λ _ key)
                                               #:build! (λ _ key)
                                               #:suppress? equal?))))
    (check-equal? (make-hash) (sys))
    (sys "a")
    (check-true (hash-has-key? (sys) "a")
                (live-build? (hash-ref (sys) "a"))))

  (test-case "Can create aliases for keys based on build products"
    (define sys (make-u/a-build-system key->key-build))
    (check-equal? (sys "marked" string? (λ (key v) (string-ref v 0)))
                  "MARKED")
    (check-eq? (sys #\M) (sys "marked"))))
