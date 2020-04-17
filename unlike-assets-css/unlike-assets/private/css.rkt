#lang racket/base

(provide (all-defined-out))
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
         [output-file-name (sha-name (open-input-string css) #".css")]))

(define (maybe-add-suffix str suff)
  (if (string-suffix? str suff)
      str
      (string-append str suff)))
