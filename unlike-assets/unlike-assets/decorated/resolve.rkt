#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/contract))
(require racket/contract
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(define (make-fence-thunk make [same? equal?])
  (let ([cache (make)])
    (λ ([next (make)])
      (begin0 (not (same? cache next))
        (set! cache next)))))

(define (make-factory-thunk make? make)
  (let ([result #f])
    (λ ()
      (when (make?) (set! result (make)))
      result)))

(define-syntax (define-uninterned-symbols stx)
  (syntax-parse stx
    [(_ x:id ...)
     (with-syntax ([(str ...)
                    (map (λ (to-str)
                           (datum->syntax to-str (symbol->string (syntax->datum to-str))))
                         (syntax->list #'(x ...)))])
     #'(define-values (x ...)
         (values (string->uninterned-symbol str)
                 ...)))]))

(define-syntax (define-private stx)
  (syntax-parse stx
    [(_ x:id [item:id c:expr] ...)
     (with-syntax ([(arg ...)
                    (map (λ (to-id)
                           (format-id to-id "~a-val" (syntax-e to-id)))
                         (syntax->list #'(item ...)))]
                   [(getter ...)
                    (map (λ (to-id)
                           (format-id to-id "~a-~a"
                                      (syntax-e #'x)
                                      (syntax-e to-id)))
                         (syntax->list #'(item ...)))]
                   [predicate (format-id #'x "~a?" #'x)])
     #'(begin
         (define-uninterned-symbols item ...)
         (provide predicate)
         (define (predicate h)
           (and (hash-eq? h)
                (hash-has-key? h item) ...))

         (provide (contract-out [x (-> c ... predicate)]))
         (define (x arg ...)
           (make-hasheq (list (cons item arg)
                              ...)))
         (begin
           (provide getter)
           (define (getter h)
             (hash-ref h item)))
         ...))]))

(define-syntax-rule (factory fence body ...)
  (make-factory-thunk (make-fence-thunk (λ () fence))
                      (λ () body ...)))
