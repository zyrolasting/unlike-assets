#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    kinda-ferpy
                    unlike-assets/reactive
                    unlike-assets/modular]]

@title{Reactive Assets as Modules}

@defmodule[unlike-assets/modular]

Under the reactive model, assets are living hashes that act as
restricted "modules". When combined with
@racket[make-u/a-build-system], you can make @deftech{procure}
procedures.  Procure procedures load non-Racket resources as if they
were dynamic Racket modules with reload support enabled.

@defproc[(make-asset [h (and/c immutable? hash?)]) procedure?]{
Returns a procedure @racket[P] such that:

@itemlist[@item{@racket[(P)] returns @racket[(hash-keys h)].}
          @item{@racket[(P k)] returns @racket[(hash-ref h k)].}
          @item{@racket[(P k t)] returns @racket[(hash-ref h k t)].}]
}

@defproc[(asset? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a value produced by @racket[make-asset].
}

@defform[(asset pair ...)
         #:grammar ([pair [id expr]])]{
A macro that expands to a @racket[make-asset] call.

@racketblock[
(asset [media-type #"text/html"]
       [version '(1 3)])]
}

@defproc[(make-asset-contract [#:allow-missing-keys? allow-missing-keys? any/c] [pair (cons/c symbol? flat-contract?)]) flat-contract?]{
Returns a contract that ensures a given asset's values match their own contracts.

If @racket[allow-missing-keys?] is a true value, then the contract
will not break if a key is not defined in the underlying asset's hash.
}

@defform[(asset/c pair ... maybe-optional-pairs)
         #:grammar [
         (pair [id contract-expr])
         (maybe-optional-pairs (code:line)
                               #:optional pair ...)]]{
The macro form of @racket[make-asset-contract]. The following
two expressions are equivalent:

@racketblock[
(or/c (make-asset-contract #:weak? #f (list (cons 'media-type bytes?)
                                            (cons 'writer (-> output-port? any))))
      (make-asset-contract #:weak? #t (list (cons 'alias string?))))]

@racketblock[
(asset/c [media-type bytes?]
         [writer (-> output-port? any)]
         #:optional
         [alias string?])]
}

@defproc[(make-u/a-procure-procedure [S u/a-build-system?])
                                     (->* (string?) #:rest symbol?)]{
Returns a @tech{procure} procedure @racketfont{P} that behaves like
a dynamic module resolver for results matching @racket[(S key asset?)].

@itemlist[
@item{@racket[(P key)] is equivalent to @racket[(S key
stateful-cell?)]. This is a @deftech{weak procurement} because it does
not wait for the result of a build, but it does start the build in the
background.}

@item{@racket[(P key sym)] is equivalent to @racket[((S key
asset?) sym)]. This is a @deftech{strong procurement} because it
starts a build, waits for the result @racket[V], and returns
@racket[(V sym)].}

@item{@racket[(P key sym . syms)] is equivalent to @racket[(apply
values (cons (P key sym) (map (lambda (s) (P key s)) syms)))].}
]

@racketblock[
(code:comment "Strong procurement")
(define html-formatted-markdown (P "index.md" 'html))

(code:comment "Weak procurement")
(define build-cell (P "index.md"))

(code:comment "Srong procurement with multiple values.")
(define-values (html-formatted-markdown output-file) (P "index.md" 'html 'out-file))
]
}
