#lang racket/base

(provide (all-defined-out)
         (all-from-out css-expr))

(require racket/file
         racket/function
         racket/path
         racket/string
         css-expr
         unlike-assets/resolver
         unlike-assets/racket-modules
         unlike-assets/files
         web-server/http/response-structs)

(define (make-style-accumulator)
  (let ([decls '()])
    (λ d (if (eq? d '())
             (string-join (map css-expr->css decls) "")
             (set! decls (append decls d))))))

(define (discard-meaningless-semicolons css-str)
  (regexp-replace* #px";\\s*\\}" css-str "}"))

(define (make-stylesheet input-path output-dir css)
  (define (write-css o)
    (write-bytes (string->bytes/utf-8 css) o))
  (asset [input-file-path input-path]
         [output-file-path
          (path-replace-extension
           (build-path output-dir
                       (make-cache-busting-file-name input-path
                                                     (open-input-string css)))
           #".css")]
         [write-file write-css]
         [->http-response
          (λ (req)
            (response/output #:code 200
                             #:mime-type #"text/css; charset=utf-8"
                             write-css))]))

(define (css-modules input-directory output-directory)
  (disjoin (racket-modules (λ (key)
                             (define modpath (simplify-path (build-path input-directory key)))
                             (and (file-exists? modpath)
                                  (equal? (path-get-extension modpath) #".rkt")
                                  (let ([get-info (call-with-input-file modpath read-language)])
                                    (and get-info
                                         (member #"text/css"
                                                 (get-info 'unlike-assets:supported-media-types null))
                                         modpath))))
                           (λ (modpath)
                             (make-stylesheet modpath output-directory
                                              (dynamic-require modpath 'css))))
           (static-files (λ (file-path)
                           (make-stylesheet file-path output-directory
                                            (file->string file-path)))
                         (λ (key)
                           (define maybe-css-file
                             (if (complete-path? key)
                                 key
                                 (build-path input-directory key)))
                           (and (equal? (path-get-extension maybe-css-file) #".css")
                                (file-exists? maybe-css-file)
                                maybe-css-file)))))

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
