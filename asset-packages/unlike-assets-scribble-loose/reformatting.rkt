#lang racket/base

(provide format-document)
(require racket/format)

; This module is responsible for grouping content
; from an *-inside Scribble reader into paragraphs.

(define (format-document $raw)
  (define-values (_ remaining) (discard-newlines $raw))
  (format-elements remaining))

(define (format-elements $raw [output '()])
  (if (eq? $raw '())
      (reverse output)
      (let-values ([(para remaining) (make-paragraph $raw)])
        (format-elements remaining
                         (if (equal? '(p) para)
                             output
                             (cons para output))))))

(define (normalize-element x)
  (if (number? x) (~a x) x))

(define (make-paragraph $raw [out '()])
  (define-values (newlines remaining) (discard-newlines $raw))
  (define (return)
    (values (apply list 'p (reverse out)) remaining))
  (if (or (eq? remaining '()) (> newlines 1))
      (return)
      (let ([x (car remaining)]
            [xs (cdr remaining)])
        (make-paragraph xs
                        (if (void? x) out
                            (case newlines
                              [(0) (cons (normalize-element x) out)]
                              [(1) (append (list (normalize-element x) " ") out)]))))))

(define (discard-newlines $raw [amt 0])
  (if (and (not (eq? $raw '())) (equal? "\n" (car $raw)))
      (discard-newlines (cdr $raw) (add1 amt))
      (values amt $raw)))
