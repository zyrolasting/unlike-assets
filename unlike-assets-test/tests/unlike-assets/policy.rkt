#lang racket/base

(require racket/class
         rackunit
         net/url
         unlike-assets/policy
         unlike-assets)

(test-equal?
 "Paths can be resolved relative to another directory"
 (build-complete-simple-path "." (current-directory))
 (current-directory))

(test-equal?
 "Paths are not left ambiguous"
 (build-complete-simple-path "b/.././" (current-directory))
 (current-directory))

(test-equal?
 "Complete paths are unmodified"
 (build-complete-simple-path (current-directory))
 (current-directory))

(test-true "Assume blank schemes are files"
           (file-url? (string->url "./blah")))

(test-true "Don't forget the obvious"
           (file-url? (string->url "file://./blah")))

(test-case "make-key->live-build/sequence"
  (define syms '(a b c d e))
  (define k->lb
    (apply make-key->live-build/sequence
           (map (λ (p) (λ (k r) (and (equal? k p) p)))
                syms)))
  (for ([p (in-list syms)])
    (check-equal? p (k->lb p void))))

(test-case "make-key->live-build/unlike-compiler"
  (define instance
    (new (class* unlike-compiler% ()
           (super-new)
           (define/override (delegate clear)
             (symbol->string clear)))))

  ; Structure logic to simulate removal on second request
  ; for symbol 'r.
  (define (available? c)
    (if (eq? c 'r)
        (not (send instance has? c))
        #t))

  (define (changed? c)
    (send instance has? c))

  (define k->lb
    (make-key->live-build/unlike-compiler
     instance available? changed?))

  (define (request key)
    ((k->lb key void) hash?))

  (check-equal? (request 'a) #hash((a . "a")))
  (check-equal? (request 'r) #hash((a . "a") (r . "r"))))
