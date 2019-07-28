#lang racket/base

(require
  racket/dict
  racket/sequence
  racket/function
  "../logging.rkt"
  "./assets.rkt"
  [prefix-in inbox: "./inbox.rkt"]
  [prefix-in outbox: "./outbox.rkt"])

(provide
  iteration-limit
  compile-unlike
  compile-all-unlike)

(define iteration-limit (make-parameter 10000))

(define (resolve-with-logger)
  (<debug "Wrapping resolver logger")
  (let ([resolve (current-resolve-unlike-proc)])
    (λ (ref)
       (define formatted-amb ((format-unclear) ref))
       (<debug "Resolving ~a" formatted-amb)
       (define asset (resolve ref))
       (<debug "Resolved ~a -> ~a"
               formatted-amb
               ((format-clear) (unlike-asset-requested-as asset)))
       asset)))

; Iterate until inbox is empty, then return the outbox.
(define (process-from-entry entry)
  (parameterize ([current-resolve-unlike-proc (resolve-with-logger)])
    (let loop ([inbox (inbox:create (unclear-ref->unlike-asset entry))]
               [outbox (outbox:create)]
               [i 0])
      (<debug "Step: ~a" i)
      (<debug "Inbox")
      (for ([i (map unlike-asset-requested-as inbox)]) (<debug "  ~a" i))
      (<debug "Outbox")
      (for ([i (dict-keys outbox)]) (<debug "  ~a" i))
      (when (> i (iteration-limit))
        (error "Iteration limit reached."))
      (if (not (inbox:empty? inbox))
        (let-values ([(new-fulfilled new-inbox)
                      (inbox:update inbox outbox)])
          (loop new-inbox
                (outbox:update outbox new-fulfilled)
                (add1 i)))
        outbox))))

(define (compile-unlike clarify resolve entry)
  (parameterize ([current-resolve-unlike-proc resolve]
                 [current-clarify-unlike-proc clarify])
    (process-from-entry entry)))

(define (compile-all-unlike clarify resolve entries)
  (let ([parent (current-thread)])
    (define threads
      (map
        (λ (entry)
           (parameterize ([prescribed-prefix (format "~a: " entry)]
                          [current-logger (make-child-logger)])
             (define outbox (with-handlers ([exn? identity])
                              (compile-unlike clarify resolve entry)))
             (thread (λ _
                        (thread-send
                          parent
                          (cons entry outbox))))))
        entries))
    (map thread-wait threads)
    (make-immutable-hash (map (λ _ (thread-receive))
                              entries))))
