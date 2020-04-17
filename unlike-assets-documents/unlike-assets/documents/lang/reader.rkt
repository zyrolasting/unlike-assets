#lang racket/base

(require racket/function
         racket/port
         syntax/strip-context)
(provide (rename-out [-read read]
                     [-read-syntax read-syntax]))

(define (-read in)
  (syntax->datum (-read-syntax #f in)))

(define (-read-syntax src in)
  (define require-spec (read-syntax src in))
  (strip-context
   #`(module reader racket/base
       (require racket/port racket/function)
       (provide (rename-out [--read read]
                            [--read-syntax read-syntax]))
       (define (--read --in)
         (syntax->datum (--read-syntax #f --in)))
       (define (--read-syntax --src --in)
         (port->list (curry read-syntax --src) --in)
           #'(module content unlike-assets/documents
               (require #,require-spec)
               code ...))))))
