#lang racket/base

(require racket/contract
         racket/function)

; I use undefined so that users can return (void) when leveraging side-effects.
(require racket/undefined)
(define undefinedλ (λ _ undefined))
(define undefined? (curry eq? undefined))

; I track asset requests for monitoring reasons.
; It also helps tailor dependency relationships.
(provide (struct-out u/a-request)
         (contract-out
          [current-u/a (parameter/c (or/c #f url?))]))

(struct u/a-request (url requestor-url))
(define current-u/a-request (make-parameter #f))

(define (make-u/a-request url)
  (u/a-request url (current-u/a)))

; The user is responsible for mapping URLs to Racket values.
(provide
 (contract-out
  [current-u/a-resolver (parameter/c (-> u/a-request? any/c))]))

(define current-u/a-resolver
  (make-parameter
   (λ _ (error 'current-u/a-resolver
               "You need to set (current-u/a-resolver)."))))

(provide
 (contract-out
  [with-dependencies (->i ([deps (non-empty-listof (or/c url? string?))]
                           [proc (deps) (procedure-arity-includes/c (length deps))])
                          [result any/c])]))

(require racket/async-channel
         racket/list
         racket/match
         racket/string
         net/url)

(struct u/a-response (ordinal value raised?))
(define (normalize-arg v)
  (if (string? v) (string->url v) v))

(define (with-dependencies urls-or-strings proc)
  (define ach (make-async-channel))
  (define urls-only (map normalize-arg urls-or-strings))
  (define num-dependencies (length urls-only))
  (define threads (map (λ (ordinal url) (u/a-load ach ordinal url))
                       (range num-dependencies)
                       urls-only))

  (apply proc
         (let loop ([loaded (build-list num-dependencies undefinedλ)])
           (match-define (u/a-response ordinal value raised?)
             (async-channel-get ach))
           (define w/new-value (list-set loaded ordinal value))

           (when raised? (raise value)) ; Keep control flow consistent.

           (if (ormap undefined? w/new-value)
               (loop w/new-value)
               w/new-value))))

(define (u/a-load ach ordinal url)
  (thread
   (λ _
     (async-channel-put
      ach
      (with-handlers ([(λ _ #t) (λ (v) (u/a-response ordinal v #t))])
        (u/a-response
         ordinal
         (u/a-resolve (make-u/a-request url))
         #f))))))

(define (u/a-resolve req)
  (parameterize ([current-u/a (u/a-request-url req)])
    ((current-u/a-resolver) req)))

; Returns a procedure that stores a singular return value.
; `hit?` controls if the stored value is returned.
(define (wrap-cache #:hit? [hit? (λ _ #t)]
                    proc)
  (let ((cache undefined))
    (λ A
      (when (or (undefined? cache)
                (not (apply hit? A)))
        (set! cache (apply proc A)))
      cache)))


(module+ test
  (require rackunit)
  (test-case "cache hits"
    (define calls 0)
    (define resp (wrap-cache
                  #:hit? (λ (v) (not (eq? v 'override)))
                  (λ (v) (set! calls (add1 calls)) v)))
    (check-equal? 'a (resp 'a))
    (check-equal? 'a (resp 'x))
    (check-equal? calls 1)
    (check-equal? 'override (resp 'override))
    (check-equal? calls 2))

  (test-case "with-dependencies"
    #|
    Assume dependency graph of this shape, such that
    <t> is the running test representing the user
    and edges point to dependencies.

    <t> ---> a
        |    ^
        |    |
        +--> b

    |#

    (define (double-the-number)
      (with-dependencies '("a")
        (curry * 2)))

    ; Computing a pseudorandom number introduces an element of change.
    ; Combine this with the cache to verify that `a` is not recomputed.
    (define respond/a (wrap-cache (λ _ (random 10))))

    (define (resolver req)
      (if (equal? "a" (url->string (u/a-request-url req)))
          (respond/a)
          (double-the-number)))

    (void
     (parameterize ([current-u/a-resolver resolver])
       (with-dependencies '("a" "b")
         (λ (a b)
           (check-equal? b (* 2 a))))))))
