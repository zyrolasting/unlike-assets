#lang racket/base

; Assets are procedures that wrap a subset of immutable hashes.
(require racket/contract
         racket/hash
         idiocket/function
         racket/match
         (for-syntax racket/base
                     syntax/parse))

(provide asset
         asset/c
         asset/p
         (contract-out [asset? predicate/c]
                       [merge-assets (->* ()
                                          (#:combine/key (-> symbol? any/c any/c any/c))
                                          #:rest (non-empty-listof asset?)
                                          asset?)]
                       [make-asset (-> (and/c immutable? hash?)
                                       (case->
                                        (-> (and/c immutable? hash?))
                                        (-> symbol? any/c)
                                        (-> symbol? any/c any)))]))

(define-values (make-asset-proc asset?) (of-name "asset"))

(define-syntax (asset/c stx)
  (syntax-parse stx
    [(n:id [x:id c:expr] ...+)
     #'(first-or/c (-> (and/c immutable? hash?))
                   (->i ([k symbol?])
                        [result (k) (case k [(x) c] ... [else any/c])]))]))

(define (make-asset h)
  (make-asset-proc
   (case-lambda [()    h]
                [(k)   (hash-ref h k)]
                [(k t) (hash-ref h k t)])))

(define (merge-assets #:combine/key [combine/key (λ (k v0 v) v)] . assets)
  (make-asset (apply hash-union
                     #:combine/key combine/key
                     (map (λ (a) (a)) assets))))

(define-syntax (asset stx)
  (syntax-parse stx
    [(n:id [x:id c:expr] ...+)
     #'(make-asset (make-immutable-hash (list (cons 'x c) ...)))]))

(define-match-expander asset/p
  (λ (stx)
    (syntax-parse stx
      [(_ x:id ...)
       #'(? asset?
            (app (λ (a) (a))
                 (hash-table ('x x) ...)))])))

(module+ test
  (require rackunit)
  (test-case "Construction and reference"
    (define a (asset [a 1] [b 2] [c 3]))
    (check-pred procedure? a)
    (check-pred asset? a)
    (check-eq? (a 'a) 1)
    (check-eq? (a 'b) 2)
    (check-eq? (a 'c) 3)
    (check-equal? (a) #hash((a . 1) (b . 2) (c . 3))))

  (test-case "Merging"
    (test-equal? "With default combine/key"
                 ((merge-assets (asset [a 1] [b 2] [c 3])
                                (asset [x 4] [y 5] [c 6])))
                 #hash((a . 1) (b . 2) (c . 6) (x . 4) (y . 5)))
    (test-equal? "With own combine/key"
                 ((merge-assets #:combine/key (λ (k v0 v) k)
                                (asset [a 1])
                                (asset [a 2])
                                (asset [a 3])))
                 #hash((a . a))))

  (test-exn "Contract enforcement"
            exn:fail:contract?
            (λ ()
              (define/contract (will-fail)
                (-> (asset/c [thunky (-> real?)]))
                (asset [thunky (λ () "not real")]))
              ((will-fail))))

  (test-equal? "Match expansion"
               (list 1 2 3)
               (match (asset [a 2] [b 3] [c 1])
                 [(asset/p a b c) (list c a b)])))
