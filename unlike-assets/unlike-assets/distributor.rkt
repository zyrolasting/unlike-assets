#lang racket/base

(require racket/contract
         unlike-assets/resolver
         racket/file
         racket/function
         racket/path
         racket/set
         racket/sequence
         "logging.rkt"
         "resolver/extension.rkt")

(provide
 (hash-partition-out distributable
                     [path complete-path?]
                     [write-file (-> output-port? (or/c void? exact-nonnegative-integer?))])
 (contract-out
  [dry-run-enabled (parameter/c boolean?)]
  [write-resolver-cache-to-filesystem!
    (->* ()
         (resolver?)
         (hash/c complete-path? (or/c exact-nonnegative-integer? void?)))]
  [sync-filesystem-to-resolver-cache!
   (->* () (resolver?) void?)]
  [write-distributable-to-filesystem!
   (-> distributable?
       (or/c exact-nonnegative-integer? void?))]))


(hash-partition distributable (path write-file))

(define dry-run-enabled (make-parameter #t))

(define (write-resolver-cache-to-filesystem! [sys (current-resolver)])
  (for/fold ([written #hash()])
            ([(a keys) (in-hash (sys))]
             #:when (distributable? (a)))
    (let ([target (a)])
      (hash-set written
                (distributable-path target)
                (write-distributable-to-filesystem! target)))))

(define (sync-filesystem-to-resolver-cache! [sys (current-resolver)])
  (define written (write-resolver-cache-to-filesystem! sys))
  (define files-written (hash-keys written))
  (define predators (apply set files-written))
  (define habitat
    (for/fold ([aggregate (set)])
              ([path files-written])
      (define dir (path-only path))
      (if (and (not (set-member? aggregate path)) (directory-exists? dir))
          (set-union aggregate (apply set (directory-list dir #:build? #t)))
          aggregate)))
  (define prey (set-subtract habitat predators))
  (sequence-for-each (λ (p) (dry (delete-directory/files p)))
                     (in-set prey)))

(define (write-distributable-to-filesystem! d)
  (define dst (distributable-path d))
  (dry (make-parent-directory* dst))
  (dry (call-with-output-file #:exists 'truncate/replace
         dst (distributable-write-file d))))

(define (log-file-operation #:level level procedure-symbol path-or-false meta)
  (log-message (current-logger)
               level
               'unlike-assets
               (format "~a => ~a"
                       procedure-symbol
                       path-or-false)
               (vector procedure-symbol
                       path-or-false
                       meta
                       (current-continuation-marks))
               #t))

(struct exn:dry-run ())

(define enforce-dry-run
  ((λ ()
     (define (make-checker meta)
       (define intentions (list->set meta))
       (λ syms (ormap (curry set-member? intentions) syms)))
     (make-security-guard (current-security-guard)
                          (λ (procedure-symbol path-or-false meta)
                            (define intended? (make-checker meta))
                            (when (intended? 'write 'delete 'execute)
                              (log-file-operation #:level 'info
                                                  procedure-symbol path-or-false meta)
                              (raise (exn:dry-run))))
                           (λ _ (error "No network access permitted for file I/O section."))
                          #f))))

(define-syntax-rule (dry b ...)
  (parameterize ([current-security-guard
                  (if dry-run-enabled enforce-dry-run (current-security-guard))])
    (with-handlers ([exn:dry-run? void])
      b ...)))
