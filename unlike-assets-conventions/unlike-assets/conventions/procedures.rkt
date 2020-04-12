#lang racket/base

(require "./contracts.rkt"
         unlike-assets/reactive
         racket/contract
         project-paths)

(provide (contract-out
          [procure/strong/relative-path-string (-> complete-path?
                                                   string?
                                                   string?)]
          [procure/weak/keep-key (-> string? string?)])
         define-abbreviated-path-lookups)

(define (procure/strong/relative-path-string input-path key)
  (path->string (find-relative-path/by-file
                 input-path
                 (with-contract procure/strong/relative-path-string
                   #:result asset/file-destined/c
                   #:freevars ([Ps (-> string? asset/file-destined/c)])
                   ((Ps key) 'output-path)))))

(define (procure/weak/keep-key key)
  (Pw key)
  key)

(define-syntax-rule (define-abbreviated-path-lookups input-path-expr)
  (begin (define Ps& (curry procure/strong/relative-path-string input-path-expr))
         (define Pw& procure/weak/keep-key)))
