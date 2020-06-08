#lang racket/base

(provide (struct-out exn:fail:unlike-assets:unresolved)
         (struct-out exn:fail:unlike-assets:cycle)
         raise-name-resolution-error
         raise-cycle-error)

(require racket/string)

(struct exn:fail:unlike-assets:unresolved exn:fail
  (name dependents))

(struct exn:fail:unlike-assets:cycle exn:fail
  (scope dependency dependents))

(define (format-dependents-list dependents)
  (format "dependents:~n~a"
          (string-join
           (map (λ (v) (format "  ~a" v)) dependents)
           "\n")))

(define (raise-name-resolution-error unresolved-name dependents)
  (raise (exn:fail:unlike-assets:unresolved
          (format "Cannot resolve name: ~v~n~a"
                  unresolved-name
                  (format-dependents-list dependents))
          (current-continuation-marks)
          unresolved-name
          dependents)))

(define (raise-cycle-error scope resolved-name dependents)
  (raise (exn:fail:unlike-assets:cycle
          (format "cycle in loading for ~v~nscope: ~v~n~a"
                  resolved-name
                  scope
                  (format-dependents-list dependents))
          (current-continuation-marks)
          scope
          resolved-name
          dependents)))
