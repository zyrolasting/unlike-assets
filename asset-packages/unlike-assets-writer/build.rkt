#lang racket/base

(provide start-build)
(require racket/file
         racket/path
         racket/set)

(define (in-assets sys)
  (in-set (apply seteq (hash-values (sys)))))

(define (start-build sys
                     #:exists [exists 'replace]
                     #:destructive [destructive? ])
  (when clean? (delete-directory/files output-directory-path))
  (for ([la (in-assets)])
    (define dst (la 'output-file))
    (make-directory* (path-only dst))
    (void (call-with-output-file #:exists exists dst (la 'write-bytes)))))
