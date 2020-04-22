#lang racket/base

(provide nearest-u/a)
(require racket/require-syntax
         (for-syntax racket/base
                     racket/require-transform
                     racket/path
                     syntax/location
                     search-upward))

(define-for-syntax (find-config start-dir)
  (search-upward/first
   (λ (d) (for/or ([f (in-list (directory-list d #:build? #t))]
                   #:when (file-exists? f))
            (call-with-input-file f
              (λ (in)
                (define get-info (read-language in (λ () #f)))
                (and (procedure? get-info)
                     (get-info 'unlike-assets:config #f)
                     f)))))
   start-dir))

(define-for-syntax (find-config/filename start-dir fn)
  (search-upward/first (file-by-exact-name fn) start-dir))

(define-for-syntax find-config/with-cache
  (let ([h (make-hash)])
    (λ (start-dir [fn #f])
      (hash-ref! h start-dir
                 (λ () (if fn
                           (find-config/filename start-dir fn)
                           (find-config start-dir)))))))

(define-for-syntax (select-find stx [fn #'#f])
  (define dir (syntax-source-directory stx))
  (unless (path? dir)
    (raise-syntax-error 'nearest-u/a
                        "Cannot find search directory. Are you using nearest-u/a in a file on disk?"))
  (define p (find-config/with-cache dir (syntax->datum fn)))
  (unless (path? p)
    (error 'nearest-u/a
           "Cannot find config file when searching from ~a" dir))
  (expand-import (datum->syntax stx `(file ,(path->string p)))))

(define-syntax nearest-u/a
  (make-require-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_) (select-find stx)]
       [(_ fn) (select-find stx #'fn)]))))
