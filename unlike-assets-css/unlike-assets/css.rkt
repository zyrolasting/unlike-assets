#lang racket/base

(provide (all-defined-out)
         (all-from-out css-expr))

(require racket/string
         css-expr
         unlike-assets/resolver
         unlike-assets/files)

(define (make-style-accumulator)
  (let ([decls '()])
    (λ d (if (eq? d '())
             (string-join (map css-expr->css decls) "")
             (set! decls (append decls d))))))

(define (discard-meaningless-semicolons css-str)
  (regexp-replace* #px";\\s*\\}" css-str "}"))

(define (make-stylesheet input-path css)
  (asset [input-file-path input-path]
         [output-file-name (make-cache-busting-file-name input-path (open-input-string css))]
         [write (λ (o) (write-bytes (string->bytes/utf-8 css)))]))

(define (maybe-add-suffix str suff)
  (if (string-suffix? str suff)
      str
      (string-append str suff)))

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

(define (font-face-src/eot url-string #:ie-fix ie-fix)
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
    #:font-weight ,weight]))
