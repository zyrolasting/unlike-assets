#lang racket/base

(module reader racket/base
  (provide (rename-out [read+ read]
                       [read-syntax+ read-syntax]))
  (require racket/port)
  (define (read+ in)
    (read-syntax+ #f in))
  (define (read-syntax+ src in)
    (with-syntax ([(code ...) (port->list read-syntax in)])
      #'(module anonymous racket/base
          (require unlike-assets/resolver
                   racket/runtime-path)
          code ...))))
