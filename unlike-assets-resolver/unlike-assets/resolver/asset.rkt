#lang racket/base

; Assets are procedures that wrap a subset of immutable hashes.
(require racket/contract
         idiocket/function
         (for-syntax racket/base
                     syntax/parse))

(provide asset/c
         asset
         (contract-out [asset? predicate/c]
                       [make-asset (-> (and/c immutable? hash?)
                                       (case->
                                        (-> (and/c immutable? hash?))
                                        (-> symbol? any/c)
                                        (-> symbol? procedure? any)))]))

(define-values (make-asset-proc asset?) (of-name "asset"))

(define-syntax (asset/c stx)
  (syntax-parse stx
    [(n:id [x:id c:expr] ...+)
     #'(and/c (-> 'x c) ...)]))

(define (make-asset h)
  (make-asset-proc
   (case-lambda [()    h]
                [(k)   (hash-ref h k)]
                [(k t) (hash-ref h k t)])))

(define-syntax (asset stx)
  (syntax-parse stx
    [(n:id [x:id c:expr] ...+)
     #'(make-asset (make-immutable-hash (list (cons 'x c) ...)))]))
