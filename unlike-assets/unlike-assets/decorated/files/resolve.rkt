#lang racket/base

(require unlike-assets/resolver
         racket/contract
         idiocket/string
         idiocket/file
         idiocket/path)

(provide (contract-out
          [find-href (-> complete-path? complete-path? string?)]
          [current-href-rewriter (parameter/c (-> string? string?))]))

(define (find-href dependent-path dependency-path)
  (path->string (find-relative-path/by-file dependent-path dependency-path)))

(define (href key)
  ((current-href-rewriter) key))

(define (procure/href dependent-path key)
  (find-href dependent-path ((procure key) 'output-file-path)))

(define current-href-rewriter (make-parameter values))
