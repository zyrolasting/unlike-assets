#lang racket/base

(require racket/contract
         idiocket/function
         racket/set
         racket/sequence
         racket/string
         "pod.rkt")

(provide
 route/c
 (struct-out exn:fail:unlike-assets:cycle)
 make-exn:fail:unlike-assets:cycle
 (contract-out
  [resolver? predicate/c]
  [make-resolver
   (->* () (#:known (hash/c string? pod?)) #:rest (non-empty-listof route/c) resolver?)]
  [invert-found (-> resolver? (hash/c pod? (non-empty-listof string?) #:immutable #t))]
  [in-found (->* (resolver? predicate/c) ((-> pod? (non-empty-listof string?) any/c)) sequence?)]))

(define-struct (exn:fail:unlike-assets:cycle exn:fail)
  (dependency-key dependents))

(define-values (make-resolver-proc resolver?) (of-name "resolver"))
(define route/c (-> string? resolver? (or/c #f pod?)))

(define (aggregate-routes . ps)
  (if (null? ps)
      (λ (k r) (error 'u/a "No pod for key: ~a" k))
      (let ([next (apply aggregate-routes (cdr ps))])
        (λ (k r)
          (or ((car ps) k r)
              (next k r))))))

(define (make-resolver #:known [table #hash()] . ps)
  (define known (hash-copy table))
  (define key->pod (apply aggregate-routes ps))
  (define unfinished (mutable-set))
  (define R
    (make-resolver-proc
     (case-lambda
       [() (hash-copy known)]
       [(key) (pod-ref key)]
       [(key stop?) (apply-until (R key) stop?)]
       [(key stop? make-alias)
        (define val (R key stop?))
        (hash-set! known (make-alias key val) (R key))
        val])))

  (define (pod-ref key)
    (raise-if-in-cycle key)
    (hash-ref! known key (λ () (key->pod key R))))

  (define (raise-if-in-cycle key)
    (define dependents (continuation-mark-set-first (current-continuation-marks) 'dependent-pods))
    (when (and (list? dependents) (member key dependents))
      (raise (exn:fail:unlike-assets:cycle
              (format "cycle in loading for ~a~ndependents:~n~a"
                      key
                      (string-join
                       (map (λ (v) (format "  ~a" v)) dependents)
                       "\n"))
              (current-continuation-marks)
              key
              dependents))))
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
                   (sequence-map (λ (p)
                                   (or (and (stop? p) p)
                                       (apply-until p stop?)))
                                 (in-hash (invert-found R)))))

(module+ test
  (require rackunit
           racket/list)

  (define (key->key-build key sys)
    (pod key #t (string-upcase key)))


  (test-case "Can aggregate routes"
    (define n 5)
    (define route (apply aggregate-routes
                         (map (λ (i) (λ (k r) (and (eq? k i) i))) (range n))))
    (for ([j (in-range n)])
      (check-eq? (route j #f) j)))

  (test-pred "Can recognize resolvers" resolver? (make-resolver void))

  (test-case "Can invert found"
    (define pA (pod "a" 'a))
    (define pB (pod "a" 'b))
    (define R (make-resolver (λ (key _) (if (< key 3) pA pB))))
    (for ([i (in-range 6)]) (R i))
    (define I (invert-found R))
    (check-equal? (sort (hash-ref I pA) <) '(0 1 2))
    (check-equal? (sort (hash-ref I pB) <) '(3 4 5))
    (check-equal? 2 (length (hash-keys I))))

  (test-case "Resolvers track encountered pods"
    (define sys (make-resolver (λ (key _) (pod key #f))))
    (check-equal? (make-hash) (sys))
    (sys "a")
    (check-true (hash-has-key? (sys) "a")
                (pod? (hash-ref (sys) "a"))))

  (test-case "Can detect cycles"
    (define sys (make-resolver (λ (key _) (pod key (sys key)))))
    (check-exn (λ (e) (and (exn:fail:unlike-assets:cycle? e)
                           (equal? (exn:fail:unlike-assets:cycle-dependency-key e) "a")
                           (equal? (exn:fail:unlike-assets:cycle-dependents e) '("a"))))
               (λ () (sys "a" (negate procedure?)))))

  (test-case "Can alias keys based on pod values"
    (define sys (make-resolver key->key-build))
    (check-equal? (sys "marked" string? (λ (key v) (string-ref v 0)))
                  "MARKED")
    (check-eq? (sys #\M) (sys "marked"))))
