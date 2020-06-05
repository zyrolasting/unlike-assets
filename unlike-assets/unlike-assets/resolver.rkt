#lang racket/base

(require racket/contract
         idiocket/function
         "cycle.rkt")

(provide
 (all-from-out "cycle.rkt")
 (struct-out exn:fail:unlike-assets:unresolved)
 (contract-out
  [resolver? predicate/c]
  [current-resolver (parameter/c resolver?)]
  [null-resolver resolver?]
  [procure (->* (any/c) (resolver?) any/c)]
  [make-resolver
   (->* ((hash/c procedure? (non-empty-listof any/c)))
        ()
        #:rest (non-empty-listof (-> any/c resolver? (or/c #f (-> any/c))))
        (and/c resolver?
               (case-> (-> (hash/c procedure? (non-empty-listof any/c) #:immutable #t))
                       (-> any/c (-> any/c)))))]))

(define-values (make-resolver-proc resolver?) (of-name "resolver"))

(struct exn:fail:unlike-assets:unresolved exn:fail (key resolver))

(define (make-resolver-error k r)
  (exn:fail:unlike-assets:unresolved
   (format "No thunk for key: ~a" k)
   (current-continuation-marks) k r))

(define (raise-resolver-error k r)
  (raise (make-resolver-error k r)))

(define (make-resolver-assert-procedure k r)
  (λ (e) (if e e (raise-resolver-error k r))))

(define (make-resolver #:rewrite-key [rewrite-key values] table . key->procs)
  (define (key->proc k r)
    (or (ormap (λ (p) (p k r))
               key->procs)
        (raise-resolver-error k r)))

  (define known
    (for*/fold ([h (make-hash)])
               ([(p ks) (in-hash table)] [k (in-list ks)])
      (hash-set! h k p)
      h))

  (define (resolve key)
    (hash-ref! known
               key
               (λ () (dependent key->proc key (key->proc key R)))))

  (define (get-manifest)
    (for/fold ([h #hash()]) ([(k p) (in-hash known)])
      (hash-set h p (cons k (hash-ref h p null)))))

  (define R
    (make-resolver-proc
     (case-lambda
       [() (get-manifest)]
       [(key) (resolve (rewrite-key key))])))

  R)

(define (procure key [resolver (current-resolver)])
  (dependent resolver key ((resolver key))))

(define null-resolver
  (make-resolver #hash() raise-resolver-error))

(define current-resolver (make-parameter null-resolver))

(module+ test
  (require rackunit
           racket/list)

  (define (key->key-build key sys)
    (λ () (string-upcase key)))

  (test-pred "Can recognize resolvers"
             resolver?
             (make-resolver #hash() void))

  (test-exn "Can detect cycles"
            exn:fail:unlike-assets:cycle?
            (λ ()
              (define R
                (make-resolver #hash()
                               (λ (key sys) (sys key))))
              (R "A")))

  (test-case "Can try multiple procedures"
    (define R
      (make-resolver (hasheq)
                     (λ (k r) (and (eq? k 3) (const 3)))
                     (λ (k r) (and (eq? k 1) (const 1)))))

    (check-exn exn:fail:unlike-assets:unresolved?
               (λ () (R 0)))
    (check-eq? ((R 1)) 1)
    (check-eq? ((R 3)) 3))

  (test-case "Resolvers track encountered values"
    (define pA (λ () 'a))
    (define pB (λ () 'b))
    (define (key->proc key _) (if (< key 3) pA pB))
    (define R (make-resolver #hash() key->proc))
    (for ([i (in-range 6)]) (R i))
    (define I (R))

    (check-equal? (sort (hash-ref I pA) <) '(0 1 2))
    (check-equal? (sort (hash-ref I pB) <) '(3 4 5))
    (check-equal? 2 (length (hash-keys I)))

    (test-equal? "A new resolver can be built from the export of another"
      I ((make-resolver I void)))))
