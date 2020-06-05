#lang racket/base

(require idiocket/function
         racket/list
         racket/set
         racket/string
         "logging.rkt")

(provide (struct-out exn:fail:unlike-assets:cycle)
         make-exn:fail:unlike-assets:cycle
         get-dependents
         get-first-dependent
         dependent)

(define-struct (exn:fail:unlike-assets:cycle exn:fail)
  (scope dependency dependents))

(define mark-key 'unlike-assets:dependents)

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
  (require rackunit)

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
