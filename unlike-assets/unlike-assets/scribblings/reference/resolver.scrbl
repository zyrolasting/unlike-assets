#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    unlike-assets]
                    "elements.rkt"]

@title{Resolvers}

@defmodule[unlike-assets/resolver]


@defthing[resolver/c  (-> any/c list? (values any/c resolver-thunk-constructor/c))]{
A procedure that represents a @tech{resolver}.

A @racket[resolver/c] procedure accepts two arguments, in this
order: an @tech{unresolved name} and a @tech{dependents list}.

The resolver must return two values. The first is a @tech{resolved
name}. The second is a procedure that represents the next step towards
a @tech{resolved value}.
}


@defthing[resolver-thunk-constructor/c (-> list? (seat/c any/c) (-> any/c))]{
A procedure built by a @tech{resolver}. It accepts a @tech{dependents list}
and a @tech{seat}, and returns a thunk. That thunk returns a @tech{resolved value}.

Both a @racket[resolver-thunk-constructor/c] procedure and the thunk it returns
are allowed to use the @tech{seat} to request resources using @tech{unresolved names}.
For this reason, they are both equipped with cycle detection. If a dependency cycle
forms, the procedure caught in a cycle will raise a @racket[exn:fail:unlike-assets:cycle].
}


@defproc[(make-resolver [make-resolved-name (-> any/c list? any/c)]
                        [make-thunk (-> any/c list? (seat/c any/c) (-> any/c))])
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

@defthing[null-resolver resolver/c]{
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
The thunks return @tech{resolved values}.
}

@defform[(seat/c contract-expr)]{
Expands to a @tech/reference{chaperone contract} that recognizes a @tech{seat},
where @tech{resolved values} match @racket[contract-expr].

Specifically: @racket[(case-> (-> seat-cache/c) (-> any/c (-> contract-expr)))].

Use in extensions that require seats to produce certain value types.
}

@defproc[(make-seat [resolver resolver/c] [cache seat-cache/c]) (seat/c any/c)]{

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

@defthing[current-seat (parameter/c (seat/c any/c)) #:value (make-seat null-resolver)]{
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
