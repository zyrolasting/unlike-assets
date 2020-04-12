#lang racket/base

(require markdown)

(define (markdown->asset key recurse)
  (define xexpr (parse-markdown src))
  (asset [input-file-path (and (file-exists? src) src)]
         [xexpr xexpr]
         [xexpr->derived ]))
