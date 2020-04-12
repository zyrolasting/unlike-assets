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

(define (local-asset-url? str)
  (and (string? str)
       (let ([inst (string->url str)])
         (and (let ([scheme (url-scheme inst)])
               (or (not scheme)
                   (equal? scheme "file:")))
              (not (url-user inst))
              (let ([host (url-host inst)])
                (or (not host)
                    (string=? host "")
                    (string=? host "localhost")
                    (string=? host "127.0.0.1")
                    (string=? host "::1")))
              (not (url-port inst))
              (empty? (url-query inst))
              (not (url-fragment inst))
              (not (empty? (url-path inst)))))))

(define (file-url? url-inst)
  (let ([scheme (url-scheme url-inst)])
    (or (not scheme)
        (equal? scheme "file"))))

(define (file-readable? path)
  (and (file-exists? path)
       (member 'read (file-or-directory-permissions path))
       #t))
