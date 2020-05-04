#lang racket/base

(require racket/contract
         idiocket/function
         "private/cycle.rkt")

(provide
 (all-from-out "private/cycle.rkt")
 (contract-out
  [resolver? predicate/c]
  [current-resolver (parameter/c resolver?)]
  [current-rewriter (parameter/c (-> any/c any/c))]
  [procure/weak (-> any/c (-> any/c))]
  [procure (-> any/c any/c)]
  [make-resolver
   (-> (hash/c procedure? (non-empty-listof any/c))
       (-> any/c resolver? (-> any/c))
       (and/c resolver?
              (case-> (-> (hash/c procedure? (non-empty-listof any/c) #:immutable #t))
                      (-> any/c (-> any/c)))))]))


(define-values (make-resolver-proc resolver?) (of-name "resolver"))

(define (make-resolver table key->proc)
  (define known
    (for*/fold ([h (make-hash)]) ([(p ks) (in-hash table)] [k (in-list ks)])
      (hash-set! h k p)
      h))

  (define (resolve key)
    (hash-ref! known
               key
               (λ ()
                 (define th (key->proc key R))
                 (λ () (dependent key (th))))))

  (define (get-manifest)
    (for/fold ([h #hash()]) ([(k p) (in-hash known)])
      (hash-set h p (cons k (hash-ref h p null)))))

  (define R
    (make-resolver-proc
     (case-lambda
       [() (get-manifest)]
       [(key) (resolve key)])))

  R)

(define (procure/weak key)
  ((current-resolver) ((current-rewriter) key)))

(define (procure key)
  ((procure/weak key)))

(define current-resolver
  (make-parameter
   (make-resolver #hash()
                  (λ (k sys)
                    (error "Use u/a to implement a resolver")))))

(define current-rewriter (make-parameter values))

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
