#lang racket/base

;;; Provides the means for relating unlike assets together.

(require racket/contract)
(provide (all-defined-out))

(require
  racket/list
  racket/path
  racket/dict
  racket/function
  "../logging.rkt")

; Use parameters to avoid carrying too many identifiers around.
(define (must-implement name) (thunk* (error "You need to implement " name)))
(define current-resolve-unlike-proc (make-parameter (must-implement "(current-resolve-unlike-proc)")))
(define current-clarify-unlike-proc (make-parameter (must-implement "(current-clarify-unlike-proc)")))

(struct dependent (val dependencies) #:transparent)
(struct unlike-asset dependent (requested-as)
        #:transparent
        #:property
        prop:procedure
        (λ (inst [available #hash()])
          (define proc (dependent-val inst))
          (define req (unlike-asset-requested-as inst))
          (define freq ((format-clear) req))
          (define objname (or (object-name proc) '<anonymous>))

          (<info "Applying ~a to ~a" objname freq)

          (unless (procedure? proc)
            (error objname "Cannot advance fulfilled asset: ~a" freq))

          (if (dependencies-met? inst available)
            (let ([dp (proc inst available)])
              (unlike-asset (dependent-val dp)
                            (dependent-dependencies dp)
                            (unlike-asset-requested-as inst)))
            (error objname "Cannot advance without fulfilled dependencies: ~a" freq))))

(define (advanceable? a available)
  (and (not (fulfilled? a))
       (dependencies-met? a available)))

(define (fulfilled? a)
  (not (procedure? (dependent-val a))))

(define (dependencies-met? a available)
   (andmap
     (lambda (req)
       (let ([clarified ((current-clarify-unlike-proc) req)])
         (and (dict-has-key? available clarified)
              (dependencies-met? (dict-ref available clarified) available))))
     (dependent-dependencies a)))

(define (unclear-ref->unlike-asset ref)
  ((current-resolve-unlike-proc) ((current-clarify-unlike-proc) ref)))
