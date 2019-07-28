#lang racket/base

(require
  racket/dict
  racket/function
  racket/sequence
  "./assets.rkt")

(provide
  create
  update
  lookup-by-unclear)

(define create make-immutable-hash)

(define (update outbox new-assets)
  (foldl
    (lambda (a res)
      (define key (unlike-asset-requested-as a))
      (when (dict-has-key? outbox key)
        (error (format "Asset key appeared twice in outbox creation: ~a" key)))
      (dict-set res key a))
    outbox
    new-assets))

(define (lookup-by-unclear unclear outbox)
  (dict-ref outbox
            ((current-clarify-unlike-proc) unclear)))


