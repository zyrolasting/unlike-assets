#lang racket/base

(require unlike-assets/resolver
         racket/contract
         net/url
         idiocket/file
         idiocket/path
         "contracts.rkt")

(provide (contract-out
          [procure/strong/relative-path-string (-> complete-path?
                                                   string?
                                                   string?)]
          [procure/weak/relative-path-string (-> string? string?)])
         define-relative-dependency-lookups)

(define (procure/strong/relative-path-string input-path key)
  (path->string (find-relative-path/by-file
                 input-path
                 (with-contract procure/strong/relative-path-string
                   #:result complete-path?
                   #:freevars ([Ps (-> string? asset/file-destined/c)])
                   ((Ps key) 'output-file-path)))))

(define (procure/weak/relative-path-string key)
  (Pw key)
  key)

(define-syntax-rule (define-relative-dependency-lookups dependent-path-expr)
  (begin (define (Ps& k) (procure/strong/relative-path-string dependent-path-expr k))
         (define Pw& procure/weak/relative-path-string)))

(define (local-file-url? str)
  (with-handlers ([exn:fail:contract? (λ _ #f)])
    (let ([inst (string->url str)])
      (and (let ([scheme (url-scheme inst)])
             (or (not scheme)
                 (equal? scheme "file:")))
           (let ([host (url-host inst)])
             (or (not host)
                 (string=? host "")
                 (string=? host "localhost")
                 (string=? host "127.0.0.1")
                 (string=? host "::1")))
           (not (null? (url-path inst)))))))
