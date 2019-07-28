#lang racket/base

(require
  rackunit
  "./assets.rkt"
  "./outbox.rkt")

(define (ast req)
  (unlike-asset #f null req))

(define (ast-pair req)
  (cons req (ast req)))

(define (make-entries . reqs)
  (create (map ast-pair reqs)))

(test-equal?
  "Merge outboxes"
  (update (make-entries "A" "B") (list (ast "C") (ast "D")))
  (make-entries "A" "B" "C" "D"))

(test-exn
  "Throw when key exists"
  exn:fail?
  (λ _ (update (make-entries "A") (list (ast "A")))))

(parameterize ([current-clarify-unlike-proc string-downcase])
  (test-case
    "Look up outbox item by unclear reference"
    (define r (ast "r"))
    (check-eq? (lookup-by-unclear "R" `#hash(("r" . ,r))) r)))
