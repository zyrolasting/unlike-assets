#lang racket/base

(require idiocket/function
         racket/list
         racket/set
         racket/string)

(provide (struct-out exn:fail:unlike-assets:cycle)
         make-exn:fail:unlike-assets:cycle
         dependent)

(define-struct (exn:fail:unlike-assets:cycle exn:fail)
  (dependency dependents))

(define mark-key 'unlike-assets:dependents)

(define (get-dependents)
  (continuation-mark-set-first
   (current-continuation-marks)
   mark-key))

(define (in-cycle? key dependents)
  (and (list? dependents)
       (member key dependents)
       #t))

(define (raise-cycle-error key dependents)
  (raise (exn:fail:unlike-assets:cycle
          (format "cycle in loading for ~a~ndependents:~n~a"
                  key
                  (string-join
                   (map (λ (v) (format "  ~a" v)) dependents)
                   "\n"))
          (current-continuation-marks)
          key
          dependents)))

(define (add-dependent key)
  (cons key (or (continuation-mark-set-first
                 (current-continuation-marks)
                 mark-key)
                null)))

(define (enter-dependent-section key proc)
  (let ([dependents (get-dependents)])
    (when (in-cycle? key dependents )
      (raise-cycle-error key dependents))
    (with-continuation-mark mark-key (add-dependent key)
      (proc))))

(define-syntax-rule (dependent key body ...)
  (enter-dependent-section key (λ () body ...)))

(module+ test
  (require rackunit)

  (define-syntax-rule (expect-cycle expected-dependency-key expected-dependencies body ...)
    (check-exn
     (λ (e) (and (exn:fail:unlike-assets:cycle? e)
                 (equal? (exn:fail:unlike-assets:cycle-dependency e) expected-dependency-key)
                 (equal? (exn:fail:unlike-assets:cycle-dependents e) expected-dependencies)))
     (λ () body ...)))

  (define-syntax-rule (expect-no-cycle body ...)
    (check-not-exn (λ () body ...)))

  (test-case "Can detect cycles"
    (expect-cycle
     "a" '("a")
     (dependent "a" (dependent "a" (void))))

    (expect-cycle
     "a" '("b" "a")
     (dependent "a" (dependent "b" (dependent "a" (void)))))

    (expect-no-cycle (dependent "a" (dependent "b" (dependent "c" (void))))))

  (test-equal? "Can trace dependents"
               (dependent "a" (dependent "b" (dependent "c" (get-dependents))))
               '("c" "b" "a")))
