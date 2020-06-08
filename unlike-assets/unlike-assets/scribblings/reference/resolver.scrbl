#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    unlike-assets]]

@title{Writing Resolvers}

@defmodule[unlike-assets/resolver]

@racketmodname[unlike-assets/resolver] provides all bindings
from @racketmodname[unlike-assets/resolver/thunk] and
@racketmodname[unlike-assets/resolver/exn].

@defthing[resolver/c  (-> any/c list? procedure? (values any/c procedure?))]{
A procedure representing a @tech{resolver}.

Since this kind of procedure is used internally by @tech{seats}, you
are not expected to use it directly. For that reason, this definition
is high-level.

A @racket[resolver/c] procedure returns two values. The first is a
@tech{resolved name}. The second is a procedure with cycle detection
enabled that returns a relevant value.
}


@defproc[(make-resolver [make-resolved-name (-> any/c list? any/c)]
                        [make-thunk (-> any/c list? seat/c (-> any/c))])
                        resolver/c]{
Returns a procedure @racket[R], which encapsulates use of
@racket[make-resolved-name] and @racket[make-thunk].

@racket[make-resolved-name] accepts an @tech{unresolved name} and a
@tech{dependents list} as arguments. It must return a @tech{resolved name}.
The value named by the first element of the dependents list
depends on the value named by the returned @tech{resolved name}.

If @racket[make-resolved-name] cannot produce a resolved name, then it
should apply @racket[raise-name-resolution-error].

@racket[make-thunk] accepts a @tech{resolved name}, a list of dependents,
and a @tech{seat}. It must return a thunk that computes a final value.

If @racket[R] is used to request a circular dependency, then it will
raise @racket[exn:fail:unlike-assets:cycle].

If @racket[R] otherwise cannot resolve a dependency, then it will
raise @racket[exn:fail:unlike-assets:unresolved].
}

@defthing[null-resolver resolver?]{
A resolver that raises @racket[exn:fail:unlike-assets:unresolved] whenever it is used.
}

@deftogether[(
@defproc[(rcons [leading resolver/c] [next resolver/c]) resolver/c]
@defproc[(rlist [element resolver/c] ...) resolver/c]
)]{
@tech{Resolver}-specific variants of @racket[cons] and @racket[list].

@racket[rcons] takes two resolvers and returns a third. The combined
resolver tries the @racket[leading] resolver first. If
@racket[leading] raises @racket[exn:fail:unlike-assets:unresolved],
then it will try the @racket[next] resolver.

@racket[rlist] combines the arguments using @racket[rcons], and
terminates the sequence using @racket[null-resolver].

The following two expressions are therefore equivalent:

@racketblock[
(rlist js css html)
(rcons js (rcons css (rcons html null-resolver)))
]
}

@defthing[seat-cache/c (hash/c any/c value-thunk/c #:immutable #t)]{
A seat cache stores @tech{resolved names} as keys, and thunks as values.
}

@defthing[seat/c (case-> (-> seat-cache/c) (-> any/c value-thunk/c))]{
A procedure that represents a @tech{seat}.
}

@defproc[(make-seat [resolver resolver/c] [cache seat-cache/c]) seat/c]{

@racket[make-seat] returns a procedure @racket[S].

@racket[S] accepts zero arguments, or one argument.

@racket[(S)] returns the current reference to the seat's cache, which
holds every encountered @tech{resolved name} as keys and every thunk
as values. The cache is populated through use of @racket[(S unresolved-name)].

@racket[(S unresolved-name)] returns the thunk produced by the
@racket[resolver].  The cache is functionally updated to hold a
@tech{resolved name} and thunk.  As a side-effect, @racket[S] discards
its reference to the prior cache.

When using @racket[resolver], @racket[S] will pass itself as an argument to
the relevant @racket[make-thunk] used in the resolver.
}

@defthing[current-seat (parameter/c seat/c) #:value (make-seat null-resolver)]{
A parameter that programs can use to share a seat.
}

@defproc[(procure/weak [unresolved-name any/c]) value-thunk/c]{
Equivalent to @racket[((current-seat) unresolved-name)].

Use this to populate a @tech{seat}'s cache while delaying the work to compute a value.
}

@defproc[(procure [unresolved-name any/c]) any/c]{
Equivalent to @racket[((procure/weak unresolved-name))].

Use this to compute a desired value from a loosely-defined name.
}
