#lang racket/base

(provide css-file->asset
         css-string->asset
         css-modules
         make-css-path)

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

(define (css-string->asset css output-dir)
  (define (write-css o)
    (write-bytes (string->bytes/utf-8 css) o))

  (hash-union
   (serveable
    (response/output #:code 200
                     #:mime-type #"text/css; charset=utf-8"
                     write-css))
   (distributable (make-css-path output-dir css)
                  write-css)))

(define (css-file->asset file-path output-dir)
  (css-string->asset (file->string file-path) output-dir))

(define (css-modules stylesheet-search-dirs output-dir)
  (existing-files (λ (p) (css-file->asset p output-dir))
                  (search-within stylesheet-search-dirs
                                 (λ (p) (equal? #".css" (path-get-extension p))))))
