#lang racket/base

; This is a version of scribble/doclang that does not restrict values
; to document parts, and allows procedures within the body.

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         apply-in-list
         format-document
         #%top-interaction
         (rename-out [#%module-begin+ #%module-begin]))

(require racket/syntax
         (only-in "reformatting.rkt" format-document)
         (only-in idiocket/list apply-in-list)
         (for-syntax racket/base
                     syntax/kerncase))

(define-syntax (#%module-begin+ stx)
  (syntax-case stx ()
    [(_ src . body)
     #'(#%module-begin
        (define input-file-path src)
        (provide input-file-path)
        (doc-begin () . body))]))

(define-syntax (doc-begin stx)
  (syntax-case stx ()
    [(_ exprs)
     #`(begin
         (define doc (format-document (list . #,(reverse (syntax->list #'exprs)))))
         (define (render) (apply-in-list doc))
         (provide render))]
    [(_ exprs . body)
     ;; Accumulate string literals from `body' to avoid trampolining on every one
     (let loop ([body #'body] [accum null])
       (syntax-case body ()
         [(s . rest) ;; Batch strings from `body' to avoid trampolining on every literal
          (string? (syntax-e #'s))
          (loop #'rest (cons #'s accum))]
         [() ;; Terminal case: Consumed everything
          (with-syntax ([(accum ...) accum])
            #`(doc-begin (accum ... . exprs)))]
         [(body1 . body) ;; A form, followed the rest of the other forms
          (with-syntax ([exprs (append accum #'exprs)])
            (let ([expanded (local-expand
                             #'body1 'module
                             (append (kernel-form-identifier-list)
                                     (syntax->list #'(provide
                                                      require))))])
              (syntax-case expanded (begin)
                [(begin body1 ...)
                 #`(doc-begin exprs body1 ... . body)]
                [(id . rest)
                 (and (identifier? #'id)
                      (ormap (lambda (kw) (free-identifier=? #'id kw))
                             (syntax->list #'(require
                                              provide
                                              define-values
                                              define-syntaxes
                                              begin-for-syntax
                                              module
                                              module*
                                              #%require
                                              #%provide
                                              #%declare))))
                 #`(begin #,expanded (doc-begin exprs . body))]
                [_else
                 #'(doc-begin (body1 . exprs) . body)])))]))]))
