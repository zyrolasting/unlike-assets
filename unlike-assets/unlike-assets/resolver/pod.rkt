#lang racket/base

(require racket/contract
         idiocket/function
         racket/undefined
         "fence.rkt"
         (for-syntax racket/base))

(provide
 pod
 (all-from-out "fence.rkt")
 (contract-out [make-pod (-> (-> (or/c #f (-> any/c))) pod?)]
               [make-pod/fenced (-> (-> any/c) (-> any/c) pod?)]
               [pod? predicate/c]))

(define-values (make-pod-proc pod?) (of-name "pod"))

(define-syntax pod
  (syntax-rules (=*)
    [(_ (=* . f) . body)
     (make-pod/fenced (fence . f)
                      (λ () . body))]
    [(_  . body)
     (pod (=* #f) . body)]))

(define (make-pod/fenced build? build)
  (make-pod (λ () (and (build?) build))))

(define (make-pod make-build)
  (define result undefined)
  (define raised #f)

  (define (on-finish res)
    (set! result res)
    (set! raised #f))

  (define (on-raised res)
    (set! result res)
    (set! raised #t))

  (define (provision)
    (define build (make-build))
    (when build
      (with-handlers ([(const #t) on-raised])
        (on-finish (build)))))

  (define (get-result)
    (if raised
        (raise result)
        result))

  (make-pod-proc
   (λ ()
     (provision)
     get-result)))

(module+ test
  (require rackunit)

  (define noop-pod (pod (void)))
  (test-pred "pod? recognizes pods" pod? noop-pod)

  (test-case "One application starts a build. A second application returns the result."
    (check-pred procedure? (noop-pod))
    (check-pred void? ((noop-pod))))

  (test-case "You can control what runs"
    (define p (pod 1))
    (check-eq? 1 ((p))))

  (test-case "A pod's wait procedure will always return the latest value"
    (define calls 0)
    (define (inc) (set! calls (add1 calls)) calls)
    (define p (make-pod (λ () inc)))
    (define wait (p))
    (check-eq? (wait) 1)
    (check-eq? wait (p))
    (check-eq? (wait) 2))

  (test-case "A pod's value will not change if you return #f."
    (define calls 0)
    (define (make-build)
      (set! calls (add1 calls))
      (and (= calls 1)
           (λ () 'never-change)))

    (define p (make-pod make-build))
    (check-eq? ((p)) 'never-change)
    (check-eq? ((p)) 'never-change)
    (check-eq? ((p)) 'never-change))

  (test-case "A pod will continue changing so long as you return a thunk."
    (define calls 0)
    (define (get-calls) calls)
    (define (make-build)
      (set! calls (add1 calls))
      get-calls)

    (define p (make-pod make-build))
    (check-eq? ((p)) 1)
    (check-eq? ((p)) 2)
    (check-eq? ((p)) 3))

  (test-case "A pod will not raise caught exceptions until you request a result"
    (define p (make-pod (λ () (λ () (error "uh-oh")))))
    (check-not-exn p)
    (check-exn exn:fail? (p)))

  (test-case "Whether an error will occur depends on the build procedure"
    (define calls 0)
    (define (make-build)
      (set! calls (add1 calls))
      (if (= calls 1)
          (λ () (error "fail"))
          (λ () 'ok)))

    (define p (make-pod make-build))
    (check-exn exn:fail? (p))
    (check-not-exn (p)))

  (test-case "A fenced pod offers a simplified interface for selecting new builds"
    (define v 0)
    (define p
      (make-pod/fenced (make-fence-thunk (λ () v))
                       (λ () v)))
    (define (inc) (set! v (add1 v)))
    (check-eq? ((p)) 0)
    (check-eq? ((p)) 0)
    (inc)
    (check-eq? ((p)) 1)
    (check-eq? ((p)) 1)
    (inc)
    (check-eq? ((p)) 2)
    (check-eq? ((p)) 2)))
