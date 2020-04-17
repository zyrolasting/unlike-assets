#lang racket/base

#|
Can you keep a secret? An asset is just a hash.

When you do something weird, like try to import a non-Racket
object as if it's a module, you have no idea what you're going
to get. Unless you want to involve fancy static analysis tricks,
you can't derive struct definitions or guarentee type safety.
Hashes are a loose tool for a loose job.

Everything in this module is about giving hashes structure with
contracts and semantic means for grabbing fields, so that you
can enforce reasonable constraints and maintain readable code.
|#
(require racket/contract)
(provide asset/c
         asset
         (contract-out [asset? (-> any/c boolean?)]
                       [make-asset (-> (and/c immutable? hash?)
                                       (case->
                                        (-> (and/c immutable? hash?))
                                        (-> symbol? any/c)
                                        (-> symbol? (-> any) any)))]
                       [make-asset-contract
                        (->* (#:allow-missing-keys? any/c
                              (non-empty-listof
                               (cons/c symbol?
                                       (-> asset? any/c))))
                             boolean?)]
                       [make-u/a-procure-procedure
                        (-> u/a-build-system? (->* (string?) #:rest (listof symbol?) any))]))

(require racket/string
         kinda-ferpy
         "model.rkt"
         (for-syntax racket/base
                     syntax/parse))

(define (make-asset-contract #:allow-missing-keys? weak? pairings)
  (λ (maybe-matching)
    (and (andmap (λ (pair)
                   (with-handlers ([exn:fail? (λ _ weak?)])
                     ((cdr pair) (maybe-matching (car pair)))))
                 pairings)
     #t)))

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
   (case-lambda [()    h]
                [(k)   (hash-ref h k)]
                [(k t) (hash-ref h k t)])
   'asset-ref))

(define-syntax (asset stx)
  (syntax-parse stx
    [(n:id [x:id c:expr] ...+)
     #'(make-asset (make-immutable-hash (list (cons 'x c) ...)))]))

(define (make-u/a-procure-procedure u/a)
  (λ (key . syms)
    (let ([la (u/a key asset?)])
      (if (null? syms)
          la
          (if (= (length syms) 1)
              (la (car syms))
              (apply values (map la syms)))))))
