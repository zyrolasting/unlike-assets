#lang racket/base

(provide (rename-out [read-syntax+ read-syntax] [read+ read])
         get-info)

(require racket/list
         racket/function
         racket/port
         syntax/strip-context)

(define (get-info in modpath line col pos)
  (λ (key default)
    (case key
      [(unlike-assets:supported-media-types) '(#"text/css")]
      [else #f])))

(define (read+ in)
  (syntax->datum (read-syntax+ #f in)))

(define (read-syntax+ src in)
  (define code (port->list (curry read-syntax src) in))
  (define-values (racket-code css-exprs)
    (splitf-at code (λ (stx) (not (eq? 'BEGIN-CSS: (syntax-e stx))))))
  (with-syntax ([srcp src] [(rktc ...) #`#,racket-code] [(cssc ...) #`#,(cdr css-exprs)])
    (strip-context
     #`(module style-asset racket/base
         (provide css)
         (require unlike-assets/css unlike-assets/resolver unlike-assets/files/resolve)
         (define add-css-expr! (make-style-accumulator))
         rktc ...
         (add-css-expr! (css-expr cssc ...))
         (define css (discard-meaningless-semicolons (add-css-expr!)))
         (module+ main (displayln css))))))
