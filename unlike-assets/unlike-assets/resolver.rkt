#lang racket/base

(require racket/contract)

; A seat manages a cache and acts as a central hub for resources.

(define seat-cache/c (hash/c any/c value-thunk/c #:immutable #t))

(define-syntax-rule (seat/c thunk-codomain)
  (case-> (-> seat-cache/c)
          (-> any/c (-> thunk-codomain))))


; A resolver does two things:
;
;   1. Create a unique name.
;   2. Create a thunk using output from #1.
;
; If a resolver cannot do #1, it will raise an exception.
; A user can combine resolvers using rcons and rlist.

(define resolver-thunk-constructor/c
  (-> list? (seat/c any/c) (-> any/c)))

(define resolver/c
  (-> any/c list? (values any/c resolver-thunk-constructor/c)))

(provide
 seat/c
 (contract-out
  [resolver/c contract?]
  [resolver-thunk-constructor/c contract?]
  [make-resolver (-> (-> any/c list? any/c)
                     (-> any/c list? value-thunk/c)
                     resolver/c)]
  [null-resolver resolver/c]
  [rcons (-> resolver/c resolver/c resolver/c)]
  [rlist (->* () #:rest (listof resolver/c) resolver/c)]
  [seat-cache/c contract?]
  [current-seat (parameter/c (seat/c any/c))]
  [make-seat (->* (resolver/c) (seat-cache/c) (seat/c any/c))]
  [procure/weak (-> any/c value-thunk/c)]
  [procure (-> any/c any/c)]))

(require "resolver/private/cycle.rkt"
         "resolver/exn.rkt"
         "resolver/thunk.rkt")

(module+ test
  (require rackunit
           racket/list
           racket/format
           racket/string))

(define (make-resolver make-resolved-name make-thunk)
  (λ (name dependents)
    (define resolved-name (make-resolved-name name dependents))
    (values resolved-name
            (λ (dependents seat)
              (define user-thunk
                (dependent make-thunk resolved-name
                           (make-thunk resolved-name dependents seat)))
                (λ () (dependent seat resolved-name (user-thunk)))))))


(define (rcons current next)
  (λ formals
    (with-handlers ([exn:fail:unlike-assets:unresolved?
                     (λ (e) (apply next formals))])
      (apply current formals))))


(define (rlist . exts)
  (if (null? exts)
      null-resolver
      (rcons (car exts)
             (apply rlist (cdr exts)))))


(define (null-resolver unresolved-name dependents)
  (raise-name-resolution-error unresolved-name dependents))


(define (make-seat resolve [cache (hash)])
  (define unresolved-name->thunk
    (λ (unresolved-name)
      (define dependents (get-dependents seat))
      (define-values (resolved-name make-thunk) (resolve unresolved-name dependents))
      (if (hash-has-key? cache resolved-name)
          (hash-ref cache resolved-name)
          (let ([val (make-thunk dependents seat)])
            (set! cache (hash-set cache resolved-name val))
            val))))

  (define seat
    (case-lambda [() cache]
                 [(unresolved-name) (unresolved-name->thunk unresolved-name)]))

  seat)

(define current-seat
  (make-parameter (make-seat null-resolver)))

(define (procure/weak unresolved-name)
  ((current-seat) unresolved-name))

(define (procure unresolved-name)
  ((procure/weak unresolved-name)))

(module+ test
  (define (capitalize&join-dependencies unresolved-name dependents)
    (if (string? unresolved-name)
        (string-join (cons (string-upcase unresolved-name) dependents) "")
        (raise-name-resolution-error unresolved-name dependents)))

  (define (make-resolved-name->symbol resolved-name dependents seat)
    (λ () (string->symbol resolved-name)))

  (define (resolve-value resolver unresolved-name [dependents null] [seat void])
    (define-values (n t) (resolver unresolved-name dependents))
    ((t dependents seat)))

  (define resolver
    (make-resolver capitalize&join-dependencies
                   make-resolved-name->symbol))

  (test-case "A resolver makes names and thunks"
    (define dependents '(" WORLD" "!"))
    (define-values (resolved-name ->thunk) (resolver "hello" dependents))

    (check-equal? resolved-name "HELLO WORLD!")
    (check-equal? ((->thunk dependents void)) '|HELLO WORLD!|))

  (test-exn "A resolver fails with an exception."
            (λ (e) (and (exn:fail:unlike-assets:unresolved? e)
                        (string-contains? (exn-message e) (~a 'blah))
                        (eq? (exn:fail:unlike-assets:unresolved-name e) 'blah)))
            (λ () (resolver 'blah null)))

  (test-exn "A resolver will catch dependency cycles when creating a thunk"
            exn:fail:unlike-assets:cycle?
            (λ ()
              (define cycles
                (make-resolver (λ (name deps) name)
                               (λ (resolved-name dependents seat)
                                 ((make-thunk dependents seat)))))
              (define-values (name make-thunk)
                (cycles 'whatever null))
              ((make-thunk null void))))

  (test-exn "A resolver will catch dependency cycles when applying a thunk"
            exn:fail:unlike-assets:cycle?
            (λ ()
              (define cycles
                (make-resolver (λ (name deps) name)
                               (λ (resolved-name dependents seat) seat)))

              (define-values (resolved-name make-thunk) (cycles 'whatever null))
              (define (nonterminating)
                ((make-thunk null nonterminating)))
              (nonterminating)))

  (test-case "Resolvers can be composed"
    ; In this case, all resolved names are strings.
    ; Each resolver resolves a name to a string from a different value type,
    ; and performs a different operation.
    (define resolve-symbols
      (make-resolver (λ (n d) (if (symbol? n)
                                  (symbol->string n)
                                  (raise-name-resolution-error n d)))
                     (λ (n d p) (λ () (string-ref n 0)))))

    (define resolve-strings
      (make-resolver (λ (n d) (if (string? n)
                                  n
                                  (raise-name-resolution-error n d)))
                     (λ (n d p) (λ () (string-upcase n)))))

    (define resolve-numbers
      (make-resolver (λ (n d) (if (number? n)
                                  (number->string n)
                                  (raise-name-resolution-error n d)))
                     (λ (n d p) (λ () (string-split n "")))))

    (define sym&null (rcons resolve-symbols null-resolver))

    (check-equal? (resolve-value sym&null 'what) #\w)

    ; Note that rcons makes it so that the exception comes from the null-resolver,
    ; since it handles an unresolved error by trying the next resolver.
    (check-exn exn:fail:unlike-assets:unresolved?
               (λ () (resolve-value sym&null "what")))

    (define str&sym&null (rcons resolve-strings sym&null))
    (check-equal? (resolve-value str&sym&null "what") "WHAT")
    (check-equal? (resolve-value str&sym&null 'what) #\w)

    (define complete (rlist resolve-strings resolve-symbols resolve-numbers))
    (check-equal? (resolve-value complete "what") "WHAT")
    (check-equal? (resolve-value complete 'what) #\w)
    (check-equal? (resolve-value complete 123) '("" "1" "2" "3" ""))
    (check-exn exn:fail:unlike-assets:unresolved?
               (λ () (resolve-value complete null))))


  ; Scenario: The home page depends a page, which in turn depends on
  ; the home page. Conflating hyperlinks with dependency relationships
  ; is an error.  The above tests catch cycles with resolvers, but we
  ; need to raise cycle errors through `procure` too.
  (test-case "Seats catch cycles"
    (define (keep-name un deps) un)
    (define (make-page-thunk r d s)
      (case r
        [("index.html") (λ () ((s "about.html")))]
        [("about.html") (λ () ((s "index.html")))]))

    (define mock-web-resolver (make-resolver keep-name make-page-thunk))
    (define seat (make-seat mock-web-resolver))

    (check-exn
     (λ (e)
       (and (exn:fail:unlike-assets:cycle? e)
                 (eq? (exn:fail:unlike-assets:cycle-scope e)
                      seat)
                 (equal? (exn:fail:unlike-assets:cycle-dependency e)
                         "index.html")
                 (equal? (exn:fail:unlike-assets:cycle-dependents e)
                         '("about.html" "index.html"))))
     (λ ()
       (parameterize ([current-seat seat])
         (procure "index.html"))))))
