#lang racket/base

(provide (all-defined-out)
         (all-from-out css-expr))

(require racket/string
         css-expr
         unlike-assets/reactive)

(define (make-style-accumulator)
  (let ([decls '()])
    (λ d (if (eq? d '())
             (string-join (map css-expr->css decls) "")
             (set! decls (append decls d))))))

(define (discard-meaningless-semicolons css-str)
  (regexp-replace* #px";\\s*\\}" css-str "}"))

(define (font-face font-family ref style weight)
  (define (&+ fmt-string)
    (& (format fmt-string ref)))
  (define eot-href (&+ "~a.eot"))
  (css-expr
   [@font-face
    #:font-family ,font-family
    #:src (apply url ,eot-href)
    #:src ((apply url ,(format "~a?#iefix" eot-href))
           (apply format "embedded-opentype"))
    ((apply url ,(&+ "~a.woff2"))
     (apply format "woff2"))
    ((apply url ,(&+ "~a.woff"))
     (apply format "woff"))
    ((apply url ,(&+ "~a.ttf"))
     (apply format "truetype"))
    #:font-style ,style
    #:font-weight ,weight
    ]))

(define (make-stylesheet input-path css)
  (asset [input-file-path input-path]
         [output-file-name (sha-name (open-input-string css) #".css")]))

; Defines a #lang read as a header and a body.
; The two are separated by exactly three dashes.
;
; HEADER
; ---
; BODY
;
; The HEADER is just racket/base.
; The BODY must be valid when it appears as (css-expr BODY)

(module reader racket/base
  (provide (rename-out [read-syntax+ read-syntax]
                       [read+ read]))
  (require racket/list
           racket/port)

  (define (read+ in)
    (read-syntax+ #f in))
  (define (read-syntax+ src in)
    (define content (port->list read in))
    (define-values (preamble rules) (splitf-at content (λ (x) (not (eq? x '---)))))

    (with-syntax ([(exprs ...) #`#,preamble]
                  [(css-exprs ...) #`#,(cdr rules)]
                  [src-string (and src (path->string src))])
      #'(module content racket/base
          (provide make-live-asset)
          (require unlike-assets/css)

          (define add-css-expr! (make-style-accumulator))

          (current-working-stylesheet
           (and (string? src-string)
                (string->path src-string)))

          exprs ...
          (add-css-expr! (css-expr css-exprs ...))
          (define css (discard-meaningless-semicolons (add-css-expr!)))
          (define (make-asset input-file)
            (make-stylesheet input-file css))
          (module+ main
            (displayln css))))))
