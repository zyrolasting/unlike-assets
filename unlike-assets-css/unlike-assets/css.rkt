#lang racket/base

(provide css-file->asset
         static-files/css)

(require idiocket/file
         racket/path
         unlike-assets/files
         unlike-assets/resolver)

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
  (file-path->asset file-path
                    output-dir
                    #:mime-type #"text/css; charset=utf-8"
                    write-css))

(define (static-files/css stylesheet-search-dirs output-dir)
  (static-files (λ (p) (css-file->asset p output-dir))
                (within-directories (λ (p) (equal? #".css" (path-get-extension p)))
                                    (if (list? stylesheet-search-dirs)
                                        stylesheet-search-dirs
                                        (list stylesheet-search-dirs)))))
