#lang racket/base

(provide (all-defined-out))
(require racket/contract
         unlike-assets/resolver)

(define asset/file-sourced/c
  (asset/c [input-file-path complete-path?]))

(define asset/file-destined/c
  (asset/c [output-file-path complete-path?]
           [write-file (-> output-port? (or/c void? exact-nonnegative-integer?))]))

(define asset/file-to-file/c
  (and/c asset/file-sourced/c
         asset/file-destined/c))
