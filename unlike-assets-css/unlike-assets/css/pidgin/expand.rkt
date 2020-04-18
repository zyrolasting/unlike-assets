#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (all-from-out unlike-assets/css
                       unlike-assets/files/resolve)
         (rename-out [-#%module-begin #%module-begin]))

(define-syntax-rule (terminal src racket-exprs css-exprs)
  (#%module-begin
    (provide make-asset)
    (define add-css-expr! (make-style-accumulator))
    (define-relative-dependency-lookups src)
    (begin . racket-exprs)
    (add-css-expr! (css-expr . css-exprs))
    (define css (discard-meaningless-semicolons (add-css-expr!)))
    (define (make-asset)
      (make-stylesheet src css))))

(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(_ src . exprs)
     (let loop ([body #'exprs] [accum null])
       (syntax-case body ()
         [()
          (with-syntax ([racket-exprs (reverse accum)])
            #'(terminal src racket-exprs ()))]
         [(head . tail)
          (eq? 'BEGIN-CSS: (syntax-e #'head))
          (with-syntax ([racket-exprs (reverse accum)])
            #'(terminal src racket-exprs tail))]
         [(head . tail)
          (loop #'tail (cons #'head accum))]))]))
