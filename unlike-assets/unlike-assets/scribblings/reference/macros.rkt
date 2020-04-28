#lang racket/base

(provide defhashpartition
         (all-from-out scribble/manual))
(require scribble/manual
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define-syntax (defhashpartition stx)
  (syntax-parse stx
    [(_ (x:id [item:id c:expr] ...) children ...)
     (with-syntax ([(arg ...)
                    (map (λ (to-id)
                           (format-id to-id "~a-val" (syntax-e to-id)))
                         (syntax->list #'(item ...)))]
                   [(getter ...)
                    (map (λ (to-id)
                           (format-id to-id "~a-~a"
                                      (syntax-e #'x)
                                      (syntax-e to-id)))
                         (syntax->list #'(item ...)))]
                   [predicate (format-id #'x "~a?" #'x)])
     #'(deftogether {
         (defproc (predicate (h hash-eq?)) boolean?)
         (defproc (x [arg c] ...) predicate)
         (defproc (getter (h predicate)) c) ...
         } children ...))]))
