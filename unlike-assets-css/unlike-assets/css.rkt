#lang racket/base

(provide (all-defined-out)
         (all-from-out css-expr))

(require racket/string
         css-expr
         unlike-assets/resolver
         unlike-assets/racket-modules)

; TODO: Target both Racket modules and CSS files.
(define (make-css-extension make-stylesheet-path)
  (make-racket-module-builder make-stylesheet-path))

(define (font-face-src url-string [format-name #f] [suffix #f])
  (define url-string/applicable
    (if suffix
        (maybe-add-suffix url-string suffix)
        url-string))
  (if format-name
      `((apply url ,url-string/applicable
        (apply format ,format-name)))
      `(apply url ,url-string/applicable)))

(define (font-face-src/woff2 url-string)
  (font-face-src url-string "woff2" ".woff2"))

(define (font-face-src/woff url-string)
  (font-face-src url-string "woff" ".woff"))

(define (font-face-src/ttf url-string)
  (font-face-src url-string "truetype" ".ttf"))

(define (font-face-src/eot url-string ie-fix)
  (define eot-href (maybe-add-suffix url-string ".eot"))
  (if ie-fix
      (font-face-src eot-href "embedded-opentype" "#iefix")
      (font-face-src eot-href #f)))

(define (font-face font-family style weight . srcs)
  (css-expr
   [@font-face
    #:font-family ,font-family
    #:src ,@srcs
    #:font-style ,style
    #:font-weight ,weight
    ]))

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

  (define (read-syntax+ src in)
    (define content (port->list read in))

    (define-values (preamble rules)
      (splitf-at content (λ (x) (not (eq? x '#:begin-css)))))

    (with-syntax ([(exprs ...) #`#,preamble]
                  [(css-exprs ...) #`#,(cdr rules)]
                  [src-string (and src (path->string src))])
      #'(module content racket/base
          (provide make-asset)
          (require unlike-assets/css
                   unlike-assets/files/resolve)
          (define add-css-expr! (make-style-accumulator))

          (define input-file
            (and (string? src-string)
                 (string->path src-string)))

          (define-relative-dependency-lookups input-file)

          exprs ...

          (add-css-expr! (css-expr css-exprs ...))
          (define css (discard-meaningless-semicolons (add-css-expr!)))
          (define (make-asset input-file)
            (make-stylesheet input-file css))

          (module+ main
            (displayln css))))))
