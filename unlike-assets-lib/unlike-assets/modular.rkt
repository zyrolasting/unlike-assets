#lang racket/base

(require racket/contract)
(provide asset/c
         asset?
         asset
         make-asset
         make-asset-contract
         raise-key-error)

(require racket/string
         kinda-ferpy
         "reactive.rkt"
         "policy.rkt"
         (for-syntax racket/base
                     syntax/parse))

(define (raise-key-error key)
  (error 'asset-ref "no such key: ~v" key))

(define (make-asset-contract #:allow-missing-keys? weak? pairings)
  (λ (maybe-matching)
    (andmap (λ (pair)
              (with-handlers ([exn:fail? (λ _ weak?)])
                ((cdr pair) (maybe-matching (car pair)))))
            pairings)))

(define-syntax (asset/c stx)
  (syntax-parse stx
    [(n:id [x:id c:expr] ...+ #:optional [ox:id oc:expr] ...+)
     #'(or/c (make-asset-contract #:allow-missing-keys? #f (list (cons 'x c) ...))
             (make-asset-contract #:allow-missing-keys? #t (list (cons 'ox oc) ...)))]
    [(n:id [x:id c:expr] ...+)
     #'(make-asset-contract #:allow-missing-keys? #f (list (cons 'x c) ...))]))

(define (asset? p)
  (eq? (object-name p) 'asset-ref))

(define (make-asset h)
  (procedure-rename
   (case-lambda [()    (hash-keys h)]
                [(k)   (hash-ref h k (λ () (raise-key-error k)))]
                [(k t) (hash-ref h k t)])
   'asset-ref))

(define-syntax (asset stx)
  (syntax-parse stx
    [(n:id [x:id c:expr] ...+)
     #'(make-asset (make-immutable-hash (list (cons 'x c) ...)))]))

(define (make-u/a-procure-procedure u/a)
  (λ (key . syms)
    (if (null? syms)
        (u/a key stateful-cell?)
        (let ([la (u/a key asset?)])
          (if (= (length syms) 1)
              (la (car syms))
              (apply values (map la syms)))))))
