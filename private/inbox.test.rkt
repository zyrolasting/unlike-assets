#lang racket/base

(require
  rackunit
  "./assets.rkt"
  "./inbox.rkt"
  "./assets.test.rkt")

(parameterize ([current-clarify-unlike-proc string-downcase]
             [current-resolve-unlike-proc w/deps])
  (test-case "Find the responsible asset"
             (define responsible (unlike-asset void null "R"))
             (define dependent (unlike-asset void '("R") "D"))
             (define irresponsible (unlike-asset void '("D") "I"))
             (check-eq? (get-next (create responsible
                                          dependent
                                          irresponsible)
                                  #hash())
                        responsible))

  (test-case "Advance the inbox via given asset"
             (define (adv data avail) (dependent void '()))
             (define asset (unlike-asset adv '() "R"))
             (define available #hash())
             (define-values (adv-asset adv-inbox) (advance (create asset) asset available))
             (define self-adv (asset))
             (check-equal? (get-next adv-inbox available) self-adv)
             (check-equal? self-adv adv-asset))

  (test-case "Expand dependencies within an inbox"
             (define asset (w/deps "R" "A" "B" "B" "C"))
             (define expanded (expand (create) asset))
             (check-true (and (contains? (w/deps "a") expanded)
                              (contains? (w/deps "b") expanded)
                              (contains? (w/deps "c") expanded)
                              (not (contains? asset expanded))))
             (define maybe-with-dupes
                 (filter
                   (lambda (a)
                     (equal? "b" (unlike-asset-requested-as a)))
                   expanded))
             (check-true (= (length maybe-with-dupes) 1)))

  (test-case "Do not expand fulfilled dependencies"
             (define r (w/deps "r"))
             (define asset (w/deps "a" "R"))
             (define expanded (expand (create) asset `#hash(("r" . ,r))))
             (check-false (contains? r expanded)))

  (test-case "Purge fulfilled assets from an inbox"
             (define A (unlike-asset #f null "A"))
             (define B (unlike-asset void null "B"))
             (define C (unlike-asset void null "C"))
             (define D (unlike-asset #f null "D"))
             (define-values (removed purged) (purge (create A B C D)))
             (check-true (and (contains? A removed)
                              (contains? D removed)))
             (check-true (and (contains? B purged)
                              (contains? C purged))))

  (test-case "Recognize resolved dependencies for upcoming assets"
    (define responsible (unlike-asset void null "R"))
    (define dependent (unlike-asset void '("R") "d"))
    (define available `#hash(("r" . ,responsible)))
    (define inbox (create dependent))
    (check-eq? (get-next inbox available) dependent)))
