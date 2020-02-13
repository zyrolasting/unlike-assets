#lang racket/base

(require racket/class racket/list racket/sequence graph)
(provide (all-defined-out))

;;  - A vertex is a clear name of an asset
;;  - A vertex property is the history of an asset
;;  - The edge property is a procedure used to regress dependent assets to an earlier state in their history
(define assets%
  (class object% (super-new)
    (define G (directed-graph '()))
    (define D (directed-graph '()))
    (define-vertex-property G history)
    (define-edge-property G ripple)

    (define/public (get-dependents v)    (get-neighbors G v))
    (define/public (get-dependencies v)  (get-neighbors D v))
    (define/public (has? v)              (has-vertex? G v))

    (define/public (get-first-advanceable)
      (findf (λ (v) (and (procedure? (lookup/latest v))
                    (dependencies-met? v)))
             (tsort G)))

    (define/private (dependencies-met? v)
      (andmap (λ (n) (not (procedure? (lookup/latest n))))
              (get-dependencies v)))

    (define/public (lookup/history clear)
      (history clear #:default #f))

    (define/public (lookup/latest clear)
      (first (or (lookup/history clear) '(#f))))

    (define/public (->hash) 
      (sequence-fold
            (λ (res k v) (hash-set res k (first v)))
            #hash()
            (in-hash (history->hash))))

    (define/public (add! v history-entry)
      (unless (has? v)
        (add-vertex! G v) (add-vertex! D v)
        (history-set! v (list history-entry))))

    (define/public (update! v initial)
      (history-set! v (list initial)))

    (define/public (progress! v history-entry)
      (history-set! v (cons history-entry (history v))))

    (define/public (regress! from to fallback)
      (define available (history to #:default '()))
      (define preferred ((ripple from to) from to available))
      (define in-history? (member preferred available))
      (history-set! to (if in-history?
                           (dropf available (λ (ua) (not (eq? ua preferred))))
                           (list fallback)))
      in-history?)

    (define/public (remove! v)
      (remove-vertex! G v)
      (remove-vertex! D v))

    (define/public (make-responsible! from to change-policy)
      (add-directed-edge! G from to)
      (add-directed-edge! D to from)
      (ripple-set! from to change-policy)
      (unless (dag? G) ; Guard against infinite loops.
        (error 'make-responsible
               "Cycle detected after new edge:~n  ~a -> ~a"
               from to)))))
