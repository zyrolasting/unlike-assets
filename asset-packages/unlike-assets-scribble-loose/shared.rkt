#lang racket/base

(provide (all-defined-out))
(require racket/function
         racket/format
         racket/list)

(define-syntax-rule (fail-to-value v body ...)
  (with-handlers ([exn:fail? (λ _ v)])
    body ...))

(define (make-text-writer txt)
  (λ (o) (write-bytes (string->bytes/utf-8 txt) o)))

; Return a procedure P such that:
;   - (P ...) caches all arguments
;   - (P) applies f to the cached arguments.
(define (apply-later f)
  (let ([cache f])
    (make-keyword-procedure
     (λ (kws kw-vals . formals)
       (if (and (null? kws) (null? kw-vals) (null? formals))
           (cache)
           (set! cache (λ () (keyword-apply f kws kw-vals formals))))))))

; Replace all procedures in the given list with the values returned
; from said procedures.  Will recurse to nested lists.
(define (apply-in-list l [out '()])
  (if (null? l)
      (reverse out)
      (let ([head (car l)])
        (apply-in-list
         (cdr l)
         (cons (cond [(procedure? head) (head)]
                     [(list? head) (apply-in-list head)]
                     [else head])
               out)))))
