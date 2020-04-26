#lang racket/base

(provide css-file->asset
         css-modules)

(require idiocket/file
         racket/path
         unlike-assets)

(define (make-css-path output-dir css)
  (path-replace-extension
   (build-path output-dir
               (make-cache-busting-file-name (if (string? css)
                                                 (open-input-string css)
                                                 css)))
   #".css"))

(define (css-file->asset file-path output-dir)
  (define (write-css o)
    (write-bytes (string->bytes/utf-8 (file->string file-path)) o))

  (hash-union
   (serveable
    (response/output #:code 200
                     #:mime-type #"text/css; charset=utf-8"
                     write-css))
   (distributable output-dir write-css)))


(define (css-modules stylesheet-search-dirs output-dir)
  (file-modules (λ (p) (css-file->asset p output-dir))
                (search-within stylesheet-search-dirs
                               (λ (p) (equal? #".css" (path-get-extension p))))))
