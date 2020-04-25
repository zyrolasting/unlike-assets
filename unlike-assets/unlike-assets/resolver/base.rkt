#lang racket/base

(require racket/contract
         idiocket/function
         racket/set
         racket/sequence
         racket/string
         "pod.rkt"
         "cycle.rkt")

(provide
 (contract-out
  [resolver? predicate/c]
  [make-resolver
   (-> (hash/c string? pod?)
       (-> string? pod?) predicate/c
       (and/c resolver?
              (case-> (-> (hash/c pod? (non-empty-listof string?) #:immutable #t))
                      (-> string? pod?))))]))

(define-values (make-resolver-proc resolver?) (of-name "resolver"))

(define (make-resolver table key->pod stop?)
  (define known (hash-copy table))

  (define (resolve-pod key)
    (λ ()
      (dependent key
                 (apply-until (key->pod key R) stop?))))

  (define (get-manifest)
    (for/fold ([inverted #hasheq()])
              ([(key pod) (in-hash known)])
      (hash-set inverted pod
                (if (hash-has-key? inverted pod)
                    (cons key (hash-ref inverted pod))
                    (list key)))))

  (define R
    (make-resolver-proc
     (case-lambda
       [() (get-manifest)]
       [(key) (hash-ref! known key (resolve-pod key))])))

  R)

(module+ test
  (require rackunit
           racket/list)

  (define (key->key-build key sys)
    (pod key #t (string-upcase key)))

  (test-pred "Can recognize resolvers"
             resolver?
             (make-resolver #hash() void pod?))

  (test-exn "Can detect cycles"
            exn:fail:unlike-assets:cycle?
            (λ ()
              (define R
                (make-resolver #hash()
                               (λ (key sys) (pod (sys key)))
                               (negate procedure?)))
              (R "A")))

  (test-case "Resolvers track encountered pods"
    (define pA (pod "a" 'a))
    (define pB (pod "a" 'b))
    (define R (make-resolver #hash() (λ (key _) (if (< key 3) pA pB)) pod?))
    (for ([i (in-range 6)]) (R i))
    (define I (R))
    (check-equal? (sort (hash-ref I pA) <) '(0 1 2))
    (check-equal? (sort (hash-ref I pB) <) '(3 4 5))
    (check-equal? 2 (length (hash-keys I)))))
