#lang racket/base

(provide (all-defined-out))
(require
  [only-in racket/generic generic-instance/c]
  [only-in net/url url?]
  racket/contract
  racket/class
  racket/path
  racket/sequence
  "../logging.rkt"
  "assets.rkt")

(define unclear/c string?)
(define clear/c (or/c string? symbol? path? url? (generic-instance/c gen:equal+hash)))
(define fulfilled/c (not/c procedure?))
(define advance/c (recursive-contract (-> clear/c (instanceof/c (subclass?/c unlike-compiler%)) unlike-asset/c)))
(define unlike-asset/c (or/c advance/c fulfilled/c))
(define ripple/c (-> clear/c clear/c (non-empty-listof unlike-asset/c) unlike-asset/c))

(define unlike-compiler%
  (class object% (super-new)
    ;; Fields -------------------------
    (define assets (new assets%))
    (define changing #f)

    ;; Public -------------------------
    (abstract delegate)
    (define/public (clarify unclear) unclear)

    (define/public (lookup clear) (send assets lookup/latest clear))
    (define/public (has? clear)   (send assets has? clear))

    (define/public (add! clear [dependent-clear #f] [ripple #f])
      (send assets add! clear (delegate clear))
      (when dependent-clear
        (send assets make-responsible!
              clear dependent-clear
              (or ripple (λ (from to history) (delegate to))))))

    (define/public (compile! #:changed [changed null]
                             #:removed [removed null]
                             #:strict? [strict? #t])
      (change-guard 'compile! (λ _
        (for ([ua removed]) (remove! ua strict?))
        (for ([ua (remove* removed changed)]) (change! ua (delegate ua) strict?))
        (let loop ()
          (define next (send assets get-first-advanceable))
          (if next
              (begin (advance! next) (loop))
              (send assets ->hash))))))


    ;; Private -------------------------
    (define/private (change-guard sym proc)
      (when changing (error sym "busy"))
      (dynamic-wind (λ _ (set! changing #t)) proc (λ _ (set! changing #f))))

    (define/private (remove! clear strict?)
      (if (has? clear)
          (begin
            (for ([dependent (send assets get-dependents clear)])
              (change! dependent (delegate dependent) #t))
            (send assets remove! clear))
          (when strict?
            (error 'remove! "~a does not exist" clear))))

    (define/private (change! clear asset strict?)
      (if (has? clear)
          (begin
            (when asset (send assets update! clear asset))
            (for ([dependent (send assets get-dependents clear)])
              (define old dependent)
              (define ripple-ok? (send assets regress! clear dependent (delegate dependent)))
              (unless ripple-ok?
                (<warning "A change to ~a did not relate to the history of ~a. Rebuilding from scratch..."
                          (format-clear clear)
                          (format-clear dependent)))
              (unless (eq? old (lookup dependent))
                (change! dependent #f #t))))
          (when strict?
            (error 'change! "~a does not exist" clear))))

    (define/private (advance! clear-name)
      (send assets progress! clear-name ((lookup clear-name) clear-name this)))))
