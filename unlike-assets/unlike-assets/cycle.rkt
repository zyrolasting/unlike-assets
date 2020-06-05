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
  (site dependency dependents))

(define mark-key 'unlike-assets:dependents)

(define (get-dependents-lookup)
  (or (continuation-mark-set-first
       (current-continuation-marks)
       mark-key)
      #hash()))

(define (get-first-dependent site)
  (let ([deps (get-dependents site)])
    (if (null? deps)
        #f
        (car deps))))

(define (get-dependents site)
  (hash-ref (get-dependents-lookup) site null))

(define (in-cycle? key dependents)
  (and (list? dependents)
       (member key dependents)
       #t))

(define (raise-cycle-error site key dependents)
  (raise (exn:fail:unlike-assets:cycle
          (format "cycle in loading for ~a~nsite: ~v~ndependents:~n~a"
                  key
                  site
                  (string-join
                   (map (λ (v) (format "  ~a" v)) dependents)
                   "\n"))
          (current-continuation-marks)
          site
          key
          dependents)))

(define (add-dependent site key)
  (define lookup (get-dependents-lookup))
  (define dependents (cons key (get-dependents site)))
  (log-message unlike-assets-logger
               'debug
               (format "dependents: ~v ~v" site dependents)
               (list site dependents))
  (hash-set lookup site dependents))

(define (enter-dependent-section site key proc)
  (let ([dependents (get-dependents site)])
    (when (in-cycle? key dependents)
      (raise-cycle-error site key dependents))
    (with-continuation-mark mark-key (add-dependent site key)
      (proc))))

(define-syntax-rule (dependent site key body ...)
  (enter-dependent-section site key (λ () body ...)))

(module+ test
  (require rackunit)

  (define-syntax-rule (expect-cycle expected-site expected-dependency-key expected-dependencies body ...)
    (check-exn
     (λ (e) (and (exn:fail:unlike-assets:cycle? e)
                 (eq? (exn:fail:unlike-assets:cycle-site e) expected-site)
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
