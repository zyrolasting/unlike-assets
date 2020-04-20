#lang racket/base

(require racket/contract
         idiocket/function
         racket/undefined
         kinda-ferpy
         (for-syntax racket/base
                     syntax/parse))

(provide
 pod
 (contract-out [make-pod
                (->* (#:sample! (-> any/c) #:build! (-> any/c any/c))
                     (#:suppress? (-> any/c any/c any/c))
                     pod?)]
               [pod? predicate/c]))

(define-values (make-pod-proc pod?) (of-name "pod"))

(define-syntax (pod stx)
  (syntax-parse stx
    [(_ body:expr)
     #'(pod [eq? x <- #f] body)]
    [(_ x:id <- sampler:expr body:expr ...+)
     #'(pod [equal? x <- sampler] body ...)]
    [(_ [suppressor:expr x:id <- sampler:expr ...+] body:expr ...+)
     #'(make-pod #:sample! (λ () sampler ...)
                 #:suppress? suppressor
                 #:build! (λ (x) body ...))]))

(define (make-pod #:sample! sample! #:suppress? [suppress? eq?] #:build! build!)
  (define initial-value (sample!))
  (define %signal (% initial-value))
  (define %producer
    (make-stateful-cell/async
     #:dependencies (list %signal)
     (λ ()
       (build! (%signal)))))
  (make-pod-proc
   (λ ([stop? stateful-cell?])
     (define next (sample!))
     (unless (suppress? (%signal) next)
       (%signal next))
     (apply-until %producer stop?))))

(module+ test
  (require rackunit)

  (test-pred "Can recognize pods"
             pod?
             (make-pod #:sample! void #:build! void))

  (test-case "Live builds start immediately"
    (define ch (make-channel))
    (define build (make-pod #:sample! void
                            #:build! (λ _ (channel-put ch 'what))))
    (define deadline (alarm-evt (+ (current-inexact-milliseconds) 500)))
    (check-eq? 'what (sync deadline ch))))
