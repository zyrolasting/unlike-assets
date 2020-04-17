#lang racket/base

(require racket/contract
         unlike-assets/resolver
         racket/file
         racket/path
         racket/set
         racket/sequence
         unlike-assets/files
         "distributor/security.rkt")

(provide
 (contract-out
  [write-asset-to-filesystem!
   (->* (asset/with-write/c)
        (#:exists symbol? #:dry-run? any/c)
        exact-positive-integer?)]
   [write-resolved-to-filesystem!
    (->* ()
         (u/a-build-system?
          #:exists symbol?
          #:dry-run? any/c)
         (hash/c complete-path? exact-positive-integer?))]
   [sync-filesystem-to-resolved! (->* () (u/a-build-system?) void?)]))

(define/under-policy (write-asset-to-filesystem! a #:exists [exists 'error])
  (define dst (a 'output-file-path))
  (make-parent-directory* dst)
  (call-with-output-file #:exists exists
    dst (a 'write-bytes)))

(define/under-policy (write-resolved-to-filesystem! [sys (current-u/a-build-system)] #:exists [exists 'error])
  (for/fold ([written #hash()])
            ([a (in-assets #:keep? asset/file-destined/c)])
    (hash-set written
              (a 'output-file-path)
              (write-asset-to-filesystem! a #:exists exists))))

(define/under-policy (sync-filesystem-to-resolved! [sys (current-u/a-build-system)])
  (define written (write-resolved-to-filesystem! sys #:exists 'truncate/replace))
  (define files-written (hash-keys written))
  (define predators (apply set files-written))
  (define habitat
    (apply set
           (map (λ (path)
                  (directory-list (path-only path)
                                  #:build? #t))
                files-written)))
  (define prey (set-subtract habitat predators))
  (sequence-for-each delete-file (in-set prey)))
