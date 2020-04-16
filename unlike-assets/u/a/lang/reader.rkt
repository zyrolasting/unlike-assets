#lang s-exp syntax/module-reader u/a

(require racket/runtime-path
         unlike-assets/resolver)

(provide (all-from-out racket/runtime-path
                       unlike-assets/resolver))
