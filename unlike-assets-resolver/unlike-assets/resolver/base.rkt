#lang racket/base

(require racket/contract
         idiocket/function
         racket/set
         racket/sequence
         "pod.rkt")

(provide
 route/c
 (contract-out
  [resolver? predicate/c]
  [make-resolver
   (->* () ((and/c hash? (not/c immutable?))) #:rest (non-empty-listof route/c) resolver?)]
  [invert-found (-> resolver? (hash/c pod? (non-empty-listof string?) #:immutable #t))]
  [in-found (->* (resolver? predicate/c) ((-> pod? (non-empty-listof string?) any/c)) sequence?)]))

(define-values (make-resolver-proc resolver?) (of-name "resolver"))
(define route/c (-> string? resolver? (or/c #f pod?)))

(define (aggregate-routes . ps)
  (if (null? ps)
      (λ (k r) (error 'u/a "No pod for key: ~a" k))
      (λ (k r)
        (or ((car ps) k r)
            (apply aggregate-routes (cdr ps))))))

(define (make-resolver [known (make-hash)] . ps)
  (define key->pod (apply aggregate-routes ps))
  (define unfinished (mutable-set))
  (define R
    (make-resolver-proc
     (case-lambda
       [() known]
       [(key) (pod-ref key)]
       [(key stop?) ((R key) stop?)]
       [(key stop? make-alias)
        (define val (R key stop?))
        (hash-set! known (make-alias key val) (R key))
        val])))
  (define (pod-ref key)
    (unless (hash-has-key? known key)
      (hash-set! known key (key->pod key R)))
    (hash-ref known key))
  R)

(define (invert-found R)
  (for/fold ([inverted #hasheq()])
            ([(key pod) (in-hash (R))])
    (hash-set inverted pod
              (if (hash-has-key? inverted pod)
                  (cons key (hash-ref inverted pod))
                  (list key)))))

(define (in-found R stop? [keep? (λ _ #t)])
  (sequence-filter keep?
                   (sequence-map (λ (pod) (or (and (stop? pod) pod)
                                              (pod stop?)))
                                 (in-hash (invert-found R)))))

(module+ test
  (require rackunit
           racket/list)

  (define (key->key-build key sys)
    (pod (string-upcase key)))

  (test-pred "Can recognize resolvers" resolver? (make-resolver void))

  (test-case "Can invert found"
    (define pA (pod 'a))
    (define pB (pod 'b))
    (define R (make-resolver (λ (key _) (if (< key 3) pA pB))))
    (for ([i (in-range 6)]) (R i))
    (define I (invert-found R))
    (check-equal? (sort (hash-ref I pA) <) '(0 1 2))
    (check-equal? (sort (hash-ref I pB) <) '(3 4 5))
    (check-equal? 2 (length (hash-keys I))))

  (test-case "Resolvers track encountered pods"
    (define sys (make-resolver (λ (key _) (pod key))))
    (check-equal? (make-hash) (sys))
    (sys "a")
    (check-true (hash-has-key? (sys) "a")
                (pod? (hash-ref (sys) "a"))))

  (test-case "Can alias keys based on pod values"
    (define sys (make-resolver key->key-build))
    (check-equal? (sys "marked" string? (λ (key v) (string-ref v 0)))
                  "MARKED")
    (check-eq? (sys #\M) (sys "marked"))))
