#lang racket/base

(require racket/contract "base.rkt" "asset.rkt" "pod.rkt")

(provide (contract-out
          [in-assets (->* () ((-> asset? (non-empty-listof string?) any/c)) sequence?)]
          [u/a (->* () #:rest (listof route/c) void?)]
          [current-resolver (parameter/c resolver?)]
          [procure (-> string? asset?)]))

(require idiocket/sequence )

(define current-resolver (make-parameter (make-resolver)))


(define route/c (-> string? resolver? (or/c #f pod?)))

(define (aggregate-routes . ps)
  (if (null? ps)
      (λ (k r) (error 'u/a "No pod for key: ~a" k))
      (let ([next (apply aggregate-routes (cdr ps))])
        (λ (k r)
          (or ((car ps) k r)
              (next k r))))))

(define (u/a . ps)
  (current-resolver
   (make-resolver #:known ((current-resolver))
                  (apply aggregate-routes ps)
                  asset?)))

(define (in-assets [keep? (λ _ #t)])
  (sequence-filter keep?
                   (sequence-map (λ (p keys)
                                   (values (procure (car keys)) keys))
                                 (in-hash (invert-found ((current-resolver)))))))

(define (procure key)
  (log-message (current-logger)
               'debug
               'unlike-assets
               (format "procure: ~a" key)
               (current-continuation-marks))
  ((current-resolver) key))


(module+ test
  (require rackunit)
  (test-case "Can aggregate routes"
    (define n 5)
    (define route (apply aggregate-routes
                         (map (λ (i) (λ (k r) (and (eq? k i) i))) (range n))))
    (for ([j (in-range n)])
      (check-eq? (route j #f) j))))
