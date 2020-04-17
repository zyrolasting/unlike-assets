#lang racket/base

(provide define/under-policy)
(require racket/function
         racket/set
         racket/string)

(struct exn:dry-run ())

(define default-dry-run (make-parameter #t))

(define (log-file-operation #:level level procedure-symbol path-or-false meta)
  (log-message (current-logger)
               'debug
               'unlike-assets/files
               (format "file op: '(~a) ~a => ~a"
                       (string-join meta " ")
                       procedure-symbol
                       path-or-false)
               (vector procedure-symbol
                       path-or-false
                       meta
                       (current-continuation-marks))))

(define (make-checker meta)
  (define intentions (list->set meta))
  (λ syms (ormap (curry set-member? intentions) syms)))

(define (make-file-i/o-policy #:dry-run? dry-run?)
  (make-security-guard (current-security-guard)
                       (λ (procedure-symbol path-or-false meta)
                         (log-file-operation #:level (if dry-run? 'info 'debug)
                                             procedure-symbol path-or-false meta)
                         (define intended? (make-checker meta))
                         (cond [(and dry-run? (intended? 'write 'delete))
                                (raise (exn:dry-run))]
                               [(intended? 'execute)
                                (error "No file executions permitted when saving files.")]
                               [(intended? 'read 'exists)
                                (void)]
                               [else (error 'make-file-i/o-policy
                                            "Incomplete security policy over effects: ~a"
                                            meta)])
                       (λ _ (error "No network access permitted for file I/O section."))
                       #f)))

(define-syntax-rule (define/under-policy (sig ...) body ...)
  (define (sig ... #:dry-run? [dry-run? (default-dry-run)])
    (with-handlers ([exn:dry-run? void])
      (parameterize ([current-security-guard (make-file-i/o-policy #:dry-run? dry-run?)]
                     [default-dry-run dry-run?])
        body ...))))
