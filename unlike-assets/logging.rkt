#lang racket/base

(provide (all-defined-out))

(define-logger unlike-assets)

(define (log-dependency-relationship scope dependency dependents)
  (log-message unlike-assets-logger
               'debug
               "Requesting dependency"
               (vector scope dependency dependents)))
