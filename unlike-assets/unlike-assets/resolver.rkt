#lang racket/base

(require racket/contract)
(provide
 (struct-out exn:fail:unlike-assets:unresolved)
 (struct-out exn:fail:unlike-assets:cycle)
 (contract-out
  [resolver? predicate/c]
  [current-resolver (parameter/c resolver?)]
  [null-resolver resolver?]
  [procure (-> any/c any/c)]
  [make-resolver
   (->* ((hash/c procedure? (non-empty-listof any/c)))
        ()
        #:rest (non-empty-listof (-> any/c any/c resolver? (or/c #f (-> any/c))))
        (and/c resolver?
               (case-> (-> (hash/c procedure? (non-empty-listof any/c) #:immutable #t))
                       (-> any/c (-> any/c)))))]))


(require racket/list
         racket/set
         racket/string
         idiocket/function
         "logging.rkt")


(module+ test
  (require rackunit
           racket/list))


; This is a duck-typing cheat that avoids use of structs.
(define-values (make-resolver-proc resolver?) (of-name "resolver"))

(define mark-key (string->uninterned-symbol "unlike-assets:dependents"))

(struct exn:fail:unlike-assets:unresolved exn:fail
  (key resolver))

(struct exn:fail:unlike-assets:cycle exn:fail
  (scope dependency dependents))

(define (raise-resolver-error k r)
  (raise (exn:fail:unlike-assets:unresolved
          (format "No thunk for key: ~a" k)
          (current-continuation-marks) k r)))

(define (make-resolver #:rewrite-key [rewrite-key values] table . key->procs)
  (define (key->proc k d r)
    (or (ormap (λ (p) (p k d r))
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
               (λ () (dependent key->proc
                                key
                                (key->proc key
                                           (get-first-dependent R)
                                           R)))))

  (define (get-manifest)
    (for/fold ([h #hash()]) ([(k p) (in-hash known)])
      (hash-set h p (cons k (hash-ref h p null)))))

  (define R
    (make-resolver-proc
     (case-lambda
       [() (get-manifest)]
       [(key)
        (let ([rewritten (rewrite-key key)])
          (dependent R rewritten
                     (resolve rewritten)))])))

  R)

(define (procure key)
  (((current-resolver) key)))

(define null-resolver
  (make-resolver #hash() raise-resolver-error))

(define current-resolver (make-parameter null-resolver))

(module+ test
  (define (key->key-build key dependent R)
    (λ () (string-upcase key)))

  (test-pred "Can recognize resolvers"
             resolver?
             (make-resolver #hash() void))

  (test-exn "Can detect cycles"
            exn:fail:unlike-assets:cycle?
            (λ ()
              (define R
                (make-resolver #hash()
                               (λ (key dep sys) (sys key))))
              (R "A")))

  (test-case "Can try multiple procedures"
    (define R
      (make-resolver (hasheq)
                     (λ (k d r) (and (eq? k 3) (const 3)))
                     (λ (k d r) (and (eq? k 1) (const 1)))))

    (check-exn exn:fail:unlike-assets:unresolved?
               (λ () (R 0)))
    (check-eq? ((R 1)) 1)
    (check-eq? ((R 3)) 3))

  (test-case "Resolvers track encountered values"
    (define pA (λ () 'a))
    (define pB (λ () 'b))
    (define (key->proc key dep _) (if (< key 3) pA pB))
    (define R (make-resolver #hash() key->proc))
    (for ([i (in-range 6)]) (R i))
    (define I (R))

    (check-equal? (sort (hash-ref I pA) <) '(0 1 2))
    (check-equal? (sort (hash-ref I pB) <) '(3 4 5))
    (check-equal? 2 (length (hash-keys I)))

    (test-equal? "A new resolver can be built from the export of another"
      I ((make-resolver I void)))))


(define (get-dependents-lookup)
  (or (continuation-mark-set-first
       (current-continuation-marks)
       mark-key)
      #hash()))

(define (get-first-dependent scope)
  (let ([deps (get-dependents scope)])
    (if (null? deps)
        #f
        (car deps))))

(define (get-dependents scope)
  (hash-ref (get-dependents-lookup) scope null))

(define (in-cycle? key dependents)
  (and (list? dependents)
       (member key dependents)
       #t))

(define (raise-cycle-error scope key dependents)
  (raise (exn:fail:unlike-assets:cycle
          (format "cycle in loading for ~a~nscope: ~v~ndependents:~n~a"
                  key
                  scope
                  (string-join
                   (map (λ (v) (format "  ~a" v)) dependents)
                   "\n"))
          (current-continuation-marks)
          scope
          key
          dependents)))

(define (add-dependent scope key)
  (define lookup (get-dependents-lookup))
  (define dependents (cons key (get-dependents scope)))
  (log-message unlike-assets-logger
               'debug
               (format "dependents: ~v ~v" scope dependents)
               (list scope dependents))
  (hash-set lookup scope dependents))

(define (enter-dependent-section scope key proc)
  (let ([dependents (get-dependents scope)])
    (when (in-cycle? key dependents)
      (raise-cycle-error scope key dependents))
    (with-continuation-mark mark-key (add-dependent scope key)
      (proc))))

(define-syntax-rule (dependent scope key body ...)
  (enter-dependent-section scope key (λ () body ...)))

(module+ test
  (define-syntax-rule (expect-cycle expected-scope expected-dependency-key expected-dependencies body ...)
    (check-exn
     (λ (e) (and (exn:fail:unlike-assets:cycle? e)
                 (eq? (exn:fail:unlike-assets:cycle-scope e) expected-scope)
                 (equal? (exn:fail:unlike-assets:cycle-dependency e) expected-dependency-key)
                 (equal? (exn:fail:unlike-assets:cycle-dependents e) expected-dependencies)))
     (λ () body ...)))

  (define-syntax-rule (expect-no-cycle body ...)
    (check-not-exn (λ () body ...)))

  (test-case "Can detect cycles"
    (expect-cycle
     0 "a" '("a")
     (dependent 0 "a" (dependent 0 "a" (void))))

    (expect-cycle
     0 "a" '("b" "a")
     (dependent 0 "a" (dependent 0 "b" (dependent 0 "a" (void)))))

    (expect-no-cycle (dependent 0 "a" (dependent 0 "b" (dependent 0 "c" (void)))))
    (expect-no-cycle (dependent 1 "a" (dependent 0 "a" (void)))))

  (test-case "Can trace dependents"
    (check-equal?
     (dependent 0 "a" (dependent 0 "b" (dependent 0 "c" (get-dependents 0))))
     '("c" "b" "a"))
    (check-false
     (get-first-dependent 0))
    (check-equal?
     (dependent 0 "a" (dependent 0 "b" (get-first-dependent 0)))
     "b")))
