#lang racket/base

;; An inbox holds a group of assets for advancement.
;; The inbox will purge any assets that are fulfilled for storage in an outbox.
;; This module implements the inbox and the means to empty it via advancing assets.
;; TODO: Use data structure optimized for search and deletion.

(require
  racket/function
  racket/dict
  racket/list
  "../logging.rkt"
  "./assets.rkt")

(provide (all-defined-out))
(provide empty?) ; from racket/list until I use a new data structure.

(define create list)

(define (get-next inbox available)
  (findf (λ (a) (advanceable? a available)) inbox))

(define (contains? elem inbox)
  (and (findf (λ (a)
                 (equal? (unlike-asset-requested-as a)
                         (unlike-asset-requested-as elem)))
               inbox)
       #t))

(define (add elem inbox)
  (cons elem inbox))

(define (delete elem inbox)
  (remove elem inbox))


; Return new inbox such that `asset` is replaced with its advanced variant.
(define (advance inbox asset available)
  (define adv-asset (asset available))
  (define adv-inbox (add adv-asset (delete asset inbox)))
  (values adv-asset adv-inbox))


; Return new inbox containing all unfulfilled dependencies of the given asset.
(define (expand inbox asset [available #hash()])
  (define unclear-refs (dependent-dependencies asset))
  (foldl
    (λ (unclear dep res)
      (if (or (contains? dep res)
              (dict-has-key? available ((current-clarify-unlike-proc) unclear)))
          res
          (add dep res)))
    inbox
    unclear-refs
    (map unclear-ref->unlike-asset unclear-refs)))


; Return all fulfilled assets in the inbox, and a new inbox with those assets removed.
(define (purge inbox)
  (values (filter fulfilled? inbox)
          (filter (negate fulfilled?) inbox)))

; Apply update cycle:
;   1. Advance an advanceable asset in the inbox
;   2. Return - all fulfilled assets resulting from step #1, and
;               a new inbox containing only unfulfilled assets, including any new resolved dependencies
(define (update inbox available)
  (define responsible (get-next inbox available))
  (define-values (adv-asset adv-inbox) (advance inbox responsible available))
  (purge (expand adv-inbox adv-asset available)))

