#lang racket/base

(require racket/contract
         racket/hash
         hash-partition)

(provide fenced-factory
         (all-from-out hash-partition
                       racket/contract
                       racket/hash)
         (contract-out [make-fence-thunk (->* ((-> any/c))
                                              ((-> any/c any/c any/c)
                                               any/c)
                                             (-> boolean?))]
                       [make-factory-thunk (-> (-> any/c) (-> any/c) (-> any/c))]))

(define (make-fence-thunk make [same? equal?] [initial #f])
  (let ([cache initial])
    (λ ([next (make)])
      (begin0 (not (same? cache next))
        (set! cache next)))))

(define make-factory-thunk
  (let ([initial (string->uninterned-symbol "initial")])
    (procedure-rename
     (λ (make? make)
       (let ([result initial])
         (λ ()
           ; Make sure (make?) runs regardless,
           ; so that things like dynamic-rerequire can run.
           (define proceed? (make?))
           (when (or (eq? result initial) proceed?)
             (set! result (make)))
           result)))
     'make-factory-thunk)))

(define (make-resolver-thunk make? make)
  (λ (k r) (and (make? k r) (make k r))))

(define-syntax-rule (fenced-factory fence factory)
  (make-factory-thunk (make-fence-thunk (λ () fence))
                      (λ () factory)))
