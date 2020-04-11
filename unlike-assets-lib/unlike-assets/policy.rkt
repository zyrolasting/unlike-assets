#lang racket/base

(require
  racket/class
  racket/list
  racket/path
  racket/dict
  racket/function
  net/url
  "logging.rkt"
  "reactive.rkt")
(provide (all-defined-out))

(define (clarify/multi compiler entries)
  (map (λ (uc) (send compiler clarify uc)) entries))

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

(define (build-complete-simple-path ref [relative-to #f])
  (let ([path (if (string? ref) (string->path ref) ref)])
    (if (complete-path? path)
        path
        (simplify-path (if (path-for-some-system? relative-to)
                           (simplify-path (build-path relative-to path))
                           path)))))

(define (chain proc . args)
  (λ (clear compiler) (apply proc args)))

(define (block r/c d/c d/h) (first d/h))
(define (rebuild r/c d/c d/h) (last d/h))

(define (make-key->live-build/sequence . maybe-makers)
  (λ (key recurse)
    (ormap (λ (p) (p key recurse))
           maybe-makers)))

(define (make-key->live-build/unlike-compiler instance available? changed?)
  (λ (key recurse)
    (define clear (send instance clarify key))
    (and clear
         (start-live-build! key
          #:sample! (λ ()
                      (define known (send instance has? clear))
                      (define available (available? clear))
                      (list (and available (not known))
                            (and (not available) known)
                            (and available known (changed? clear))))
          #:suppress? (λ (a b) (ormap values b))
          #:build! (λ (spec)
                     (define a (car spec))
                     (define r (cadr spec))
                     (define c (caddr spec))
                     (when a
                       (send instance add! clear))
                     (send instance compile!
                           #:changed (if c (list clear) null)
                           #:removed (if r (list clear) null)))))))
