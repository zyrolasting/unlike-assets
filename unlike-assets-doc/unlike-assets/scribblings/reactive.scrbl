#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    kinda-ferpy
                    unlike-assets/reactive
                    unlike-assets/policy
                    unlike-assets]]

@title{Reactive Model}

@defmodule[unlike-assets/reactive]

The reactive model uses my @racketmodname[kinda-ferpy] library to track
dependencies between assets and build them both lazily and
asynchronously. This model is best for those who prefer
functional programming and don't want to deal with the
underlying graph.

@bold{This module is experimental. Use the imperative model if you
need a stable interface.} Once this stabilizes, I suggest you prefer
it over the imperative model.

@section{Reactive API Reference}
@defproc[(u/a-build-system? [p any/c]) boolean?]{
Returns @racket[#t] if @racket[p] is an unlike asset build system created by @racket[make-u/a-build-system].
}

@defproc[(live-build? [p any/c]) boolean?]{
Returns @racket[#t] if @racket[p] is a live build created by @racket[start-live-build!].
}

@defproc[(start-live-build! [key string?]
                            [#:sample! sample! (-> any/c)]
                            [#:build! build!  (-> any/c any/c)]
                            [#:suppress? suppress? (-> any/c any/c any/c?) eq?])
  live-build?]{
Returns a procedure @racket[LB] that encapsulates a value
that changes due to external factors.

When you apply @racket[start-live-build!], you also apply
@racket[sample!], then @racket[build!]. @racket[build!]
will run on its own thread.

Specifically, this procedure creates two stateful cells:

@itemlist[
@item{@racket[%S], which holds the last known value of @racket[(sample!)].}
@item{@racket[%B], which is an async cell created using @racket[(make-stateful-cell/async #:dependencies (list %S)
(lambda () (build! (%S))))].}
]

Each time you apply @racket[LB], it will apply @racket[sample!] and compare
the returned value to @racket[%S] using @racket[suppress?]. If
@racket[suppress?] returns a false value, then @racket[%S] will
update to the new value and cause @racket[%B] to update.

@racket[(LB)] will return @racket[%B]. You can depend on this cell to
monitor when the build thread restarts.

@margin-note{Why have @racket[stop?] at all? Because @racket[(%B)]
returns a procedure that waits for the build thread to finish. This
means @racket[((%B))] waits for the build output. If that build output
is a procedure--which is normal when resolving dependencies--then
getting to the final value means writing a comical expression with at
least three bracket pairs: @racket[(((%B)))]. @racket[stop?]
allows you to search for a common representation of build output.}
@racket[(LB stop?)] will apply @racket[(stop? %B)], then
@racket[(stop? (%B))], @racket[(stop? ((%B)))], and so on until
it encounters a true result. The value @racket[V] that made @racket[(stop? V)]
true value is the value returned. This implies that @racket[(LB)] is equivalent
to @racket[(LB stateful-cell?)].
}

@defproc[(make-u/a-build-system [key->live-build (-> any/c u/a-build-system? live-build?)]
                                [known (and/c hash? (not/c immutable?)) (make-hash)])
                                u/a-build-system?]{
Returns a procedure @racket[S] that encapsulates @racket[known],
a mutable hash of live builds encountered through use of @racket[S].

@racket[(S)] evaluates to @racket[known].

@racket[(S key)] will return a live build with the given key.  If the
build does not already exist in @racket[known], then it will first be
added using @racket[(key->live-build key S)]. Subsequent applications
of @racket[S] to @racket[key] will return the same reference to the
live build without consulting @racket[key->live-build]. Notice that
@racket[S] can be invoked recursively in the body of
@racket[key->live-build]. This allows one live build to depend on
others. Be warned that if a cycle forms, then the build will not
terminate.

@racket[(S key stop?)] behaves like @racket[(S key)], but will also
apply the live build procedure @racket[LB] to @racket[stop?] (See
@racket[start-live-build!]).

@margin-note{@racket[(S key stop? make-alias)] is useful for clients
that want to refer to a live build by the name of its product.  If an
asset @racket["main.css.rkt"] produces @racket["f871a234.css"], then
@racket[(S "f871a234.css")] will return the same live build that
produced it. Note that this does NOT return the same version of the
asset the name implies! It returns the living build and therefore only
the latest version, even if the name is outdated.}  @racket[(S key
stop? make-alias)] behaves like @racket[(S key stop?)], with the added
behavior of creating an alias for @racket[key] based on the produced
value. @racket[make-alias] is a procedure that accepts two formals and
returns an alias for @racket[key]. The arguments are always
@racket[key] and @racket[(S key stop?)], in that order. If
@racket[make-alias] produces a key that already exists, that value for
that key will be overwritten. Once evaluated, @racket[(eq? (S key) (S
(make-alias key (S key stop?))))] is true.
}

@section{Reactive Assets as Modules}

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

@section{Example: Living Values based on Files}

@racketblock[
(require racket/runtime-path)

(code:comment "Base change detection on file modification seconds.")
(define (start-live-file-build! key build!)
  (start-live-build! key
    #:sample!   (λ () (file-or-directory-modify-seconds key))
    #:suppress? =
    #:build!    build!))

(define (start-live-line-count key u/a)
  (start-live-file-build! key
                          (λ (mtime) (length (file->lines key)))))

(define (start-live-mtime key u/a)
  (start-live-file-build! key identity))

(define (key->live-build key u/a)
  (define start!
    (if (string-suffix? key ".txt")
        start-live-line-count
        start-live-mtime))
  (start! key u/a))

(define sys (make-u/a-build-system key->live-build))

(code:comment "This will return the current modification time of dummy.bin.")
(sys "dummy.bin" number?)
(code:comment "This will return the current number of lines in passage.txt.")
(sys "passage.txt" number?)
]
