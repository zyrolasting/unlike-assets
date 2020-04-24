#lang racket/base

(require unlike-assets/resolver
         racket/contract
         idiocket/string
         idiocket/file
         idiocket/path
         "contracts.rkt")

(provide (contract-out
          [find-href (-> complete-path? complete-path? string?)]
          [procure/strong/href (-> complete-path? string? string?)]
          [procure/weak/href (-> string? string?)]
          [current-procure/href (parameter/c (-> string? string?))]
          [procure/href (-> string? string?)]))

(define (find-href dependent-path dependency-path)
  (path->string (find-relative-path/by-file dependent-path dependency-path)))

(define (procure/href key)
  ((current-procure/href) key))

(define (procure/strong/href dependent-path key)
  (define dependency-path
    ((Ps key) 'output-file-path #f))

  (unless (complete-path? dependency-path)
    (error 'procure/strong/href
           (lines->string
            "Asset does not define a complete path for 'output-file-path"
            "  key: ~a"
            "  got: ~a"
            "(assuming #f when value is not set)")
           key
           dependency-path))

  (find-href dependent-path dependency-path))

(define (procure/weak/href key)
  (Pw key)
  key)

(define current-procure/href (make-parameter procure/weak/href))
