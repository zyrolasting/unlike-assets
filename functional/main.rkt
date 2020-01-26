#lang racket/base

;; Responsible for mapping URLs to state graph nodes.

(provide (all-defined-out))

(require racket/promise
         net/url
         "state-graph.rkt")

;; The user is responsible for mapping URLs to Racket values.
(define url->stateful-cell
  (make-parameter
   (λ _ (error 'current-u/a-resolver
               "You need to set (url->stateful-cell)."))))

(define (u/a-resolve url)
  ((url->stateful-cell)
   (if (string? url)
       (string->url url)
       url)))

(struct message (url datum))

(define (make-stateful-graph-thread url->v^)
  (let ([parent-thread (current-thread)])
    (thread (λ _
              (parameterize ([url->stateful-cell url->v^])
                (let loop ()
                  (match-define (message url datum) (thread-receive))
                  (let ([cell (u/a-resolve url)])
                    (thread-send parent-thread
                                 (message url
                                          (cell datum))))
                  (loop)))))))
