#lang racket/base

(require rackunit
         unlike-assets/reactive)

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
  (check-eq? (sys #\M) (sys "marked")))

(test-case "A shared build system is available"
  (test-equal? "Ps := procure/strong" procure/strong Ps)
  (test-equal? "Pw := procure/weak" procure/weak Pw)
  (test-exn "The shared system requires you to map keys to live builds"
            exn:fail?
            (current-key->live-build))

  (define (key->asset-build key sys)
    (start-live-build! key
                       #:sample! (λ _ #f)
                       #:build! (λ _ (asset [key key]
                                            [up (string-upcase key)]))
                       #:suppress? equal?))

  (parameterize ([current-key->live-build key->asset-build])
    (check-equal? (Ps "a" 'up) "A")))
