#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    "lib.rkt"]]

@(define (tech/reference str)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))

@title{Unlike Assets}

@declare-exporting["lib.rkt"]

Let's say you write a Racket module that requires an SVG document
called @racket{logo.svg} to work. Unlike Assets (UA) allows you to
express that requirement as a dependency. Instead of writing
@racket[require], you write @racket[procure], as in @racket[(procure
"logo.svg")].

UA allows you to define what @racket[(procure "logo.svg")] means in
your programs. @racket{logo.svg} is an @tech{unresolved name}. An
@deftech{unresolved name} is any Racket value used as input to a
@tech{resolver}.

A @deftech{resolver} is a procedure that maps an @tech{unresolved
name} to a @deftech{resolved name} and a procedure that returns a
@deftech{resolved value}. Both resolved names and resolved values
can also be any Racket value.

For example, a resolver could translate @racket{logo.svg} into a
complete path and a procedure that returns an X-expression
representing the SVG's contents.

Resolvers can be composed, so that a resolver that handles SVG
documents can forward requests to a resolver that handles JavaScript
modules.

A @deftech{seat} provides an entry point and cache for resolvers.  It
helps ensure that values are only (re)loaded when necessary, and
allows a resolver to request its own dependencies. This way, the
aforementioned JavaScript resolver can depend on resources from the
SVG resolver.

@racket[procure] is just a front-end to @racket[(current-seat)]. That
means that UA comes down to making a @tech{resolver}, making a
@tech{seat} for that resolver, and putting that seat in
@racket[current-seat] when needed. This controls how @racket[procure]
translates its sole argument to something more useful.

As a bonus, UA adds cycle detection support to all resolvers. It uses
a @deftech{dependents list} when considering if a dependency cycle has
occurred. A dependents list is a list of @tech{resolved names}, such
that a value computed from the name in position @tt{N} is dependent on
the value computed from the name in position @tt{N-1}. The value named
by the first element is dependent on a value that is not yet
represented in the dependents list. A programmer should interpret this
gap using @racket[exn:fail:unlike-assets:cycle] or
@racket[make-resolver].


@section{Resolvers}

@defthing[resolver/c  (-> any/c list? (values any/c resolver-thunk-constructor/c))]{
A procedure used as a @tech{resolver}.

A @racket[resolver/c] procedure accepts two arguments, in this
order: an @tech{unresolved name} and a @tech{dependents list}.

The resolver must return two values. The first is a @tech{resolved
name}. The second is a @racket[resolver-thunk-constructor/c]
procedure.
}

@defthing[resolver-thunk-constructor/c (-> list? (seat/c any/c) (-> any/c))]{
A procedure built by a @tech{resolver}. It accepts a @tech{dependents
list} and a @tech{seat}, and returns a thunk. That thunk returns a
@tech{resolved value}.

Both a @racket[resolver-thunk-constructor/c] procedure and the thunk
it returns are allowed to use the @tech{seat} to recursively request
resources using @tech{unresolved names}.  For this reason, they are
both equipped with cycle detection. If a dependency cycle forms, the
procedure caught in a cycle will raise a
@racket[exn:fail:unlike-assets:cycle].
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

@section{Seats}

@defthing[seat-cache/c (hash/c any/c (-> any/c) #:immutable #t)]{
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

@defproc[(procure/weak [unresolved-name any/c]) (-> any/c)]{
Equivalent to @racket[((current-seat) unresolved-name)].

Use this to populate a @tech{seat}'s cache while delaying the work to compute a value.
}

@defproc[(procure [unresolved-name any/c]) any/c]{
Equivalent to @racket[((procure/weak unresolved-name))].

Use this to compute a desired value from a loosely-defined name.
}


@section{Errors}

@defstruct*[(exn:fail:unlike-assets:unresolved exn:fail) ([name any/c] [dependents list?])]{
An error raised when a resolver could not produce a @tech{resolved name}.

@racket[name] is a reference to the exact name a user passed to a
resolver.

@racket[dependents] is a @tech{dependents list}. Assuming the name
resolution error did not occur at all, the value named by the first
element would be dependent on the resolved variant of @racket[name].

}

@defstruct*[(exn:fail:unlike-assets:cycle exn:fail) ([scope procedure?] [dependency any/c] [dependents any/c])]{
An error raised when a @tech{resolver} or a thunk it produced
encounted a circular dependency.

@racket[scope] is a procedure that would never terminate if it weren't
for this exception.

@racket[dependency] is a @tech{resolved name} of a resource that is
already a dependent, and therefore cannot be resolved.

@racket[dependents] is a @tech{dependents list}, where the value named
by the first element is dependent on the value named by
@racket[dependency].

}


@defproc[(raise-name-resolution-error [name any/c] [dependents list?] [message string? "Cannot resolve name [...]"]) any]{
Raises @racket[exn:fail:unlike-assets:unresolved] with the given
arguments. The default @racket[message] shows @racket[name] and a
formatted view of @racket[dependents].
}
