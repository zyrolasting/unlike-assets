#lang racket/base

(require racket/contract
         unlike-assets/resolver
         racket/file
         racket/path
         racket/set
         racket/sequence
         "../files.rkt"
         "distributor/security.rkt")

(provide
 (contract-out
  [write-asset-to-filesystem!
   (->* (asset/file-destined/c)
        (#:exists symbol? #:dry-run? any/c)
        (or/c exact-nonnegative-integer? void?))]
   [write-resolved-to-filesystem!
    (->* ()
         (resolver?
          #:exists symbol?
          #:dry-run? any/c)
         (hash/c complete-path? (or/c exact-nonnegative-integer? void?)))]
   [sync-filesystem-to-resolved!
    (->* ()
         (resolver?
          #:dry-run? any/c)
         void?)]))

(define/under-policy (write-asset-to-filesystem! a #:exists [exists 'error])
  (define dst (a 'output-file-path))
  (log-info "Saving asset: ~a" dst)
  (make-parent-directory* dst)
  (call-with-output-file #:exists exists
    dst (a 'write-file)))

(define/under-policy (write-resolved-to-filesystem! [sys (current-resolver)] #:exists [exists 'error])
  (log-info "Write resolved")
  (for/fold ([written #hash()])
            ([(a keys) (in-assets (λ (a keys)
                                    (and (complete-path? (a 'output-file-path #f))
                                         (procedure? (a 'write-file)))))])
    (hash-set written
              (a 'output-file-path)
              (write-asset-to-filesystem! a #:exists exists))))

(define/under-policy (sync-filesystem-to-resolved! [sys (current-resolver)])
  (define written (write-resolved-to-filesystem! sys #:exists 'truncate/replace))
  (define files-written (hash-keys written))
  (define predators (apply set files-written))
  (define habitat
    (apply set
           (apply append
                  (map (λ (path)
                         (directory-list (path-only path)
                                         #:build? #t))
                       files-written))))

  (define prey (set-subtract habitat predators))
  (sequence-for-each (λ (p) (dry (delete-file p)))
                     (in-set prey)))
