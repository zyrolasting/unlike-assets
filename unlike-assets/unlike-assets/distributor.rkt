#lang racket/base

(require racket/contract
         unlike-assets/resolver
         racket/file
         racket/path
         racket/set
         racket/sequence
         "distributor/security.rkt"
         "resolver/extension.rkt")

(provide
 (hash-partition-out
  distributable
  [path complete-path?]
  [write-file (-> (or/c void? exact-nonnegative-integer?))])
 (contract-out
  [write-asset-to-filesystem!
   (->* (distributable?)
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

(hash-partition distributable (path write-file))

(define/under-policy (write-asset-to-filesystem! a #:exists [exists 'error])
  (define dst (distributable-path a))
  (dry (make-parent-directory* dst))
  (dry (call-with-output-file #:exists exists
         dst (distributable-write-file a))))

(define/under-policy (write-resolved-to-filesystem! [sys (current-resolver)] #:exists [exists 'error])
  (for/fold ([written #hash()])
            ([(a keys) (in-hash (sys))]
             #:when (distributable? a))
    (hash-set written
              (distributable-path a)
              (write-asset-to-filesystem! a #:exists exists))))

(define/under-policy (sync-filesystem-to-resolved! [sys (current-resolver)])
  (define written (write-resolved-to-filesystem! sys #:exists 'truncate/replace))
  (define files-written (hash-keys written))
  (define predators (apply set files-written))
  (define habitat
    (for/fold ([aggregate (set)])
              ([path files-written])
      (define dir (path-only path))
      (if (and (not (set-member? aggregate path)) (directory-exists? dir))
          (set-union aggregate (apply set (directory-list #:build? #t)))
          aggregate)))

  (define prey (set-subtract habitat predators))
  (sequence-for-each (λ (p) (dry (delete-file p)))
                     (in-set prey)))
