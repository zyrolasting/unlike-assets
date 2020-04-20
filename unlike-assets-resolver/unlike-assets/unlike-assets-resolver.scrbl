#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    racket/match
                    racket/rerequire
                    kinda-ferpy
                    unlike-assets/resolver]]

@title{Unlike Assets: Resolver}
@author{Sage Gerard}

@defmodule[unlike-assets/resolver]

@racketmodname[unlike-assets/resolver] provides all bindings from
@racketmodname[unlike-assets/resolver/base],
@racketmodname[unlike-assets/resolver/pod],
@racketmodname[unlike-assets/resolver/asset], and
@racketmodname[unlike-assets/resolver/global].

This section will cover each binding provided by all of these modules.

@section{Asset Resolver}
@defmodule[unlike-assets/resolver/global]

This module provides a process-wide interface for
@racketmodname[unlike-assets/resolver/base],
@racketmodname[unlike-assets/resolver/pod], and
@racketmodname[unlike-assets/resolver/asset]. You can create your own
resolver in terms of those modules.

@defthing[current-resolver (parameter/c resolver?)]{
This is a global resolver used by @racket[procure] to find updated
Racket values. By default, it raises an error that it cannot
resolve any key. You will need to provide an implementation by
setting this parameter, possibly with @racket[u/a].
}

@defproc[(procure/weak [key string?]) stateful-cell?]{
Equivalent to @racket[((current-resolver) key)].  This starts an
asynchronous build for an asset (if needed), but does not wait for the
result.
}

@defproc[(procure [key string?] [sym symbol?] ...) any/c]{
This starts a build for an asset (if needed), waits for the results,
then returns requested data.
}

@deftogether[(
@defthing[Pw procure/weak]
@defthing[P procure]
)]{
You'll probably use the @tt{procure/*} procedures often enough to want
these abbreviations.
}

@defproc[(u/a [route route/c] ...) void?]{
Imperatively replaces @racket[current-resolver] with a new resolver
built with the given routes. This preserves the hash table used by the
existing resolver.
}

@section{Mapping Names to Living Racket Values}
@defmodule[unlike-assets/resolver/base]

@defproc[(resolver? [p any/c]) boolean?]{
Returns @racket[#t] if @racket[p] came from @racket[make-resolver].
}

@defthing[route/c contract? #:value (-> string? resolver? (or/c #f pod?))]{
Matches procedures that map strings to @tech{pods}.
}

@defproc[(make-resolver [found (and/c hash? (not/c immutable?)) (make-hash)]
                        [find-pod route/c] ...)
                        resolver?]{
Returns a procedure @racket[R] that manages but does not encapsulate
@racket[found]. @racket[R] will store @tech{pods} it discovers in @racket[found].

@racket[R]'s behavior depends on the number of provided arguments:

@margin-note{Use @racket[(R key return? make-alias)] to find a pod with
an old value. For example: If the pod found by @racket{main.css.rkt} produces
stylesheet @racket{f871a234.css}, then @racket[make-alias] can cause
@racket[(R "f871a234.css")] to return the pod that produced that stylesheet.}
@itemlist[
@item{@racket[(R)] evaluates to @racket[found]. Deleting keys from
@racket[found] will not be harmful assuming no thread-safety issues, but it
will force the resolver to recompute the value of the missing key if it
is requested again.}

@item{@racket[(R key)] will return @racket[(hash-ref found key)] if
the key exists. Otherwise the key will first be set to the first
non-@racket[#f] result from @racket[(find-pod key R)], for each given
@racket[find-pod] in order. @racket[R] may be invoked recursively in
any @racket[find-pod].}

@item{@racket[(R key return?)]: Like @racket[((R key) return?)] (See @racket[make-pod]).
If a pod refers to itself via a cycle, then this will not terminate.}

@item{@racket[(R key return? make-alias)]: Like @racket[(R key return?)].
As a side effect, makes the following true: @racket[(eq? (R key) (R (make-alias key (R key return?))))].
}
]
}

@defproc[(invert-found [R resolver?]) (hash/c pod? (non-empty-listof string?) #:immutable #t)]{
Returns a @racket[hasheq] hash that represents the inverse of
@racket[(R)], meaning each key is a pod and each value is a list
of keys used to refer to the respective pod.
}


@defproc[(in-found [R resolver?]
                   [return? predicate/c pod?]
                   [keep? (-> any/c (non-empty-listof string?) any/c)
                   (const #t)]) sequence?]{
Returns a two-value sequence filtered by @racket[keep?].

The first value is a pod @racket[P], if @racket[(return? P)] is true.
Otherwise, it's @racket[(P return?)].

The second value is a list of all keys that can be used to access the
value in @racket[R]'s managed hash.
}


@section{Pods}
@defmodule[unlike-assets/resolver/pod]

@deftech{Pods} keep Racket values up-to-date, asynchronously.

@defproc[(pod? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] came from @racket[make-pod].
}

@defproc[(make-pod [#:sample! sample! (-> any/c)]
                   [#:build! build!  (-> any/c any/c)]
                   [#:suppress? suppress? (-> any/c any/c any/c) eq?])
  (and/c pod?
         (-> predicate/c any/c))]{
Returns a procedure @racket[P] that governs a changing value.

When you apply @racket[make-pod], it will immediately evaluate
@racket[(sample!)] and remember the value. It will then apply
@racket[build!] to that value in a new thread.

When you apply @racket[P], it checks @racket[(suppress? (sample!)
previous)], where @racket[previous] is the last value returned from
@racket[sample!].  If the result if @racket[#f], then it will run
@racket[build!] on a new thread that replaces the original build
result and thread.

@margin-note{Why have @racket[return?] at all? Because many
@tt{unlike-assets-*} packages capture changes in dependencies using
chains of thunks. @racket[return?] allows you to search for a common
representation of build output without ridiculous expressions like
@racket[((((((P))))))].}
@racket[P] accepts one formal parameter called @racket[return?].  The
value of @racket[(P stateful-cell?)] is a @racketmodname[kinda-ferpy]
cell @racket[C], whose value is a procedure that waits for and returns
@racket[(build!)]. In general, @racket[(P return?)]  is @racket[(or
(return? C) (return? (C)) (return? ((C))) ...)].
}


@defform*[((pod [suppress? id <- sample ...] expr ...)
           (pod id <- sample1 expr ...)
           (pod expr))]{
Creates a pod. The first form expands directly to a call to
@racket[make-pod].  As an example, the following two expressions are
equivalent:

@racketblock[
(pod [= v <- (check-outside-world)
             (return-something)]
     (build-value-with v))

(make-pod #:suppress? =
          #:sample! (lambda ()
                     (check-outside-world)
                     (return-something))
          #:build! (lambda (v) (build-value-with v)))
]

The @racket[<-] notates a binding between @racket[id] and the result
of the @racket[sample ...] body. It can only be one value.

The first abbreviation has exactly one term before @racket[<-], and
one fewer pair of brackets. It uses the first of the terms following
@racket[<-] as the body of a @racket[#:sample!] argument, and
uses @racket[equal?] as the argument to @racket[#:suppress?]

@racketblock[
(pod mtime <- (file-or-directory-modify-seconds path)
     (displayln mtime))

(code:comment "becomes...")
(make-pod #:suppress? equal?
          #:sample! (lambda () (file-or-directory-modify-seconds path))
          #:build! (lambda (mtime) (displayln mtime)))]

The last abbreviation uses a single expression for a non-changing build.
This is useful for tests and corner cases.

@racketblock[
(pod 1)

(code:comment "becomes...")
(make-pod #:suppress? eq?
          #:sample! (lambda () #f)
          #:build! (lambda (v) 1))]
}


@section{Assets Definitions}
@defmodule[unlike-assets/resolver/asset]

Assets are just hashes with lipstick.

@defproc[(make-asset [h (and/c immutable? hash?)]) procedure?]{
Returns a procedure @racket[P] such that:

@itemlist[@item{@racket[(P)] returns @racket[h].}
          @item{@racket[(P k)] returns @racket[(hash-ref h k)].}
          @item{@racket[(P k t)] returns @racket[(hash-ref h k t)].}]
}

@defproc[(asset? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a value produced by @racket[make-asset].
}

@defform[(asset pair ...)
         #:grammar ([pair [id expr]])]{
A macro that expands to a @racket[make-asset] call.

These two expressions are equivalent:

@racketblock[
(asset [media-type #"text/plain"]
       [nums '(1 3)])]

@racketblock[
(make-asset (hash 'media-type #"text/plain"
                  'nums '(1 3)))]
}

@defform[(asset/c pair ...)
         #:grammar [(pair [id contract-expr])]]{
Creates a contract that captures individual values in an asset.

@racketblock[
(asset/c [media-type bytes?]
         [writer (-> output-port? any)])]
}

@defform[(asset/p id ...)]{
A @racket[match] pattern for assets, @italic{not} the hashes they
decorate. You do not have to list every key for an asset, but every key
you list must exist in the asset's hash for the pattern to match.

@racketblock[
(match-define (asset/p flavor) (asset [flavor 'chocolate] [kind 'ice-cream]))
(eq? flavor 'chocolate)

(match-define (asset/p vendor) (asset [flavor 'chocolate] [kind 'ice-cream]))
(code:comment "match-define: no matching clause for #<procedure:asset>")
]
}
