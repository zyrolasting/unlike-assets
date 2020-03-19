#lang racket/base

(require rackunit
         unlike-assets/reactive)

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
  (define sys (make-u/a-build-system
               (λ (key _) (start-live-build! key
                                             #:sample! (λ _ #f)
                                             #:build! (λ _ (string-upcase key))
                                             #:suppress? equal?))))
  (check-equal? (sys "marked" string? (λ (key v) (string-ref v 0)))
                "MARKED")
  (check-eq? (sys #\M) (sys "marked")))
