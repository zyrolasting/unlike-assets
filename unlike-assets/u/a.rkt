#lang racket/base

(require (for-syntax racket/base))
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [#%module-begin+ #%module-begin]))

(define-syntax (#%module-begin+ stx)
  (syntax-case stx ()
    [(_ body ...)
     #'(#%module-begin
        (require racket/runtime-path
                 unlike-assets/resolver)
        body ...)]))
