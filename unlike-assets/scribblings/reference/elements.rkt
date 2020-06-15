#lang racket/base

(provide (all-defined-out))

(require scribble/manual)

(define (tech/reference str)
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))
