#lang racket/base

(require racket/contract
         idiocket/function
         racket/undefined
         "fence.rkt"
         (for-syntax racket/base))

(provide
 pod
 (all-from-out "fence.rkt")
 (contract-out [make-pod (-> string? (-> (or/c #f (-> any/c))) pod?)]
               [make-pod/fenced (-> string? (-> any/c) (-> any/c) pod?)]
               [pod? predicate/c]))

(define-values (make-pod-proc pod?) (of-name "pod"))

(define-syntax pod
  (syntax-rules (=*)
    [(_ key (=* . f) . body)
     (make-pod/fenced key
                      (fence . f)
                      (λ () . body))]
    [(_ key . body)
     (pod key (=* #f) . body)]))

(define (make-pod/fenced key build? build)
  (make-pod key
            (λ () (and (build?) build))))

(define (make-pod key make-build)
  (define build #f)
  (define worker #f)
  (define result undefined)
  (define raised #f)

  (define (add-dependent)
    (cons key (or (continuation-mark-set-first
                   (current-continuation-marks)
                   'dependent-pods)
                  null)))

  (define (on-finish res)
    (set! result res)
    (set! raised #f))

  (define (on-raised res)
    (set! result res)
    (set! raised #t))

  (define (main mark)
    (with-continuation-mark 'dependent-pods mark
      (with-handlers ([(const #t) on-raised])
        (on-finish (build)))))

  (define (provision mark)
    (set! build (make-build))
    (when build
      (when (thread? worker)
        (break-thread worker))
      (set! worker (thread (λ () (main mark))))))

  (define (wait-for-result)
    (when (thread? worker)
      (thread-wait worker))
    (if raised
        (raise result)
        result))

  (make-pod-proc
   (λ ()
     (provision (add-dependent))
     wait-for-result)))

(module+ test
  (require rackunit)

  (define (test-pod b) (make-pod "" b))
  (define (test-podλ b [key ""]) (make-pod key (const b)))
  (define noop-pod (test-podλ #f))

  (test-pred "pod? recognizes pods" pod? noop-pod)

  (test-case "One application starts a build. A second application waits for a result."
    (check-pred procedure? (noop-pod))
    (test-eq? "Results start as undefined"
              undefined ((noop-pod))))

  (test-case "You can control what runs in a new thread"
    (define p (test-podλ (const 1)))
    (check-eq? 1 ((p))))

  (test-case "Pods can be traced"
    (define p
      (make-pod "p"
                (const (λ ()
                         (continuation-mark-set-first (current-continuation-marks)
                                                      'dependent-pods)))))

    ; The pods must provision a thread in the dynamic extent of a
    ; procedure returned by another pod's make-build. Otherwise the
    ; dependency relationship won't be tracked.
    (define a (make-pod "a" (const (λ () ((p))))))
    (define b (make-pod "b" (const (λ () ((a))))))
    (define c (make-pod "c" (const (λ () ((b))))))

    (check-equal? ((c)) '("p" "a" "b" "c")))

  (test-case "A pod's wait procedure will always return the latest value"
    (define calls 0)
    (define (inc) (set! calls (add1 calls)) calls)
    (define p (test-podλ inc))
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

    (define p (test-pod make-build))
    (check-eq? ((p)) 'never-change)
    (check-eq? ((p)) 'never-change)
    (check-eq? ((p)) 'never-change))

  (test-case "A pod will continue changing so long as you return a thunk."
    (define calls 0)
    (define (get-calls) calls)
    (define (make-build)
      (set! calls (add1 calls))
      get-calls)

    (define p (test-pod make-build))
    (check-eq? ((p)) 1)
    (check-eq? ((p)) 2)
    (check-eq? ((p)) 3))

  (test-case "A pod will not raise caught exceptions until you request a result"
    (define (crash) (error "uh-oh"))
    (check-not-exn (λ () ((test-podλ crash))))
    (check-exn exn:fail? ((test-podλ crash))))

  (test-case "Whether an error will occur depends on the build procedure"
    (define calls 0)
    (define (make-build)
      (set! calls (add1 calls))
      (if (= calls 1)
          (λ () (error "fail"))
          (λ () 'ok)))

    (define p (test-pod make-build))
    (check-exn exn:fail? (p))
    (check-not-exn (p)))

  (test-case "A fenced pod offers a simplified interface for selecting new builds"
    (define v 0)
    (define p
      (make-pod/fenced ""
                       (make-fence-thunk (λ () v))
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
