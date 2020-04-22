#lang racket/base

(require unlike-assets/resolver
         racket/contract
         net/url
         idiocket/file
         idiocket/path
         "contracts.rkt")

(provide (contract-out
          [find-href (-> complete-path? complete-path? string?)]
          [procure/href (-> complete-path? string? string?)]
          [procure/weak/href (-> string? string?)])
         define-relative-dependency-lookups)

(define (find-href dependent-path dependency-path)
  (path->string (find-relative-path/by-file dependent-path dependency-path)))

(define (procure/href input-path key)
  (find-href input-path
             (Ps key 'output-file-path
                 #:make-alias (λ (k a) (find-href input-path (a 'output-file-path))))))

(define (procure/weak/href key)
  (Pw key)
  key)

(define-syntax-rule (define-relative-dependency-lookups dependent-path-expr)
  (begin (define (Ps& k) (procure/href dependent-path-expr k))
         (define Pw& procure/weak/href)))

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
