#lang racket/base

(provide markdown->asset)
(require markdown
         txexpr
         dynamic-xml
         unlike-assets/files)

(define (get-dependent-attr x)
  (ormap (λ (k) (and (attr-ref x k #f) k))
         '(src srcset href)))

(define (markdown->asset input-file-path output-directory)
  (define xexpr/base     (start-markdown-document input-file-path))
  (define xexpr/expanded (expand-custom-elements xexpr/base))
  (define xexpr/final    (resolve-dependencies xexpr/expanded))
  (define html-string    (xexpr->string xexpr/final))

  (define (write-all-bytes out)
    (write-bytes (open-input-string html-string)))

  (asset [input-file-path input-file-path]
         [output-file-path
          (build-path output-directory
                      (path-replace-extension
                       (file-name-from-path input-file-path)
                       #".html"))]
         [write-all-bytes write-all-bytes]
         [->http-response
          (response/output #:code 200
                           #:mime-type #"text/html; charset=utf-8"
                           write-all-bytes)]))

(define (start-markdown-document input-file-path)
  `(html (head (title "Untitled"))
         (body . ,(parse-markdown input-file-path))))

(define (expand-custom-elements xexpr/base)
  (define-values (xexpr/no-scripts racket-elements)
    (splitf*-txexpr xexpr/base
                    (λ (x) (and (list? x)
                                (equal? (get-tag x) 'script)
                                (equal? "application/racket"
                                        (attr-ref x 'type #f))))
                    (λ (x) #f)))
  (foldl
   (λ (racket-el res)
     (define tmp-file (make-temporary-file))
     (dynamic-wind
       void
       (λ ()
         (call-with-output-file tmp-file
           (λ (o) (displayln (string-join (filter string? (get-elements racket-el)) ""))))
         (apply-xexpr-element res
                              (dynamic-require tmp-file 'transformers)))
       (λ () (delete-file tmp-file))))
   xexpr/no-scripts
   racket-elements))

(define (resolve-dependent-element input-file-path use? maybe-dependent)
  (let* ([key (get-dependent-attr maybe-dependent)]
         [val (and key (attr-ref maybe-dependent key))])
    (if (use? val)
        (attr-set maybe-dependent
                  (Ps& input-file-path uri))
        maybe-dependent)))
