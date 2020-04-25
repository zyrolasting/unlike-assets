#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    unlike-assets/resolver]]

@title{@tt{unlike-assets/resolver}}
@defmodule[unlike-assets/resolver]

This module helps you write dynamic variants of @racket[require]. That
is, import procedures that are not limited to @racket[require]'s data
formats and conventions.  In this light, a non-Racket resource can
present itself as a Racket value via @racket[procure].

In the context of this module, a @deftech{resolver} is a procedure
that cooperates with a surjective mapping of Racket values to
thunks. A resolved value is, however, a non-procedure.

Resolvers in the wild normally understand protocols, search
directories, and other conventions. The resolvers defined here have no
behavior beyond enforcing a particular flow of control.

@defproc[(resolver? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] came from @racket[make-resolver].
}

@defproc[(make-resolver [known (hash/c procedure? (non-empty-listof string?))]
                        [key->thunk (-> any/c resolver? (-> any/c))])
                        (and/c resolver?
                               (case-> (-> (hash/c procedure?
                                                   (non-empty-listof string?)
                                                   #:immutable #t))
                                       (-> any/c (not/c procedure?))))]{
Returns a procedure @racket[R] that encapsulates a mutable cache based
on @racket[known]. @racket[R]'s behavior depends on the number of
provided arguments.

@racket[(R)] returns a @racket[hasheq] @racket[H] that maps all cached
@racket[(key->thunk name R)] values to a list of each corresponding
@racket[name].

@racketblock[
(R "a") (R "b") (R "c")
(R) (code:comment "#hash((#<procedure:x> . '(\"a\" \"b\")) (#<procedure:y> . '(\"c\")))")
]

In this example, @racket[(R "a")] and @racket[(R "b")] will both apply
@racket[x] to compute a value.

You can create a new resolver based on another resolver's cache using
the following pattern:

@racketblock[
(define R+ (make-resolver (R) key->thunk))
]

Reclaiming memory entails discarding resolver references as well
as resolved value references before a garbage collection pass.

@racket[(R key)] caches @racket[(key->thunk key R)], if it has not
already done so. The return value is computed as follows:

@racketblock[
(let loop ([p cached-procedure])
  (if (procedure? p)
      (loop (p))
      p))
]

This accounts for the degrees of seperation between an abstract,
dependent value and a concrete, independent value. A user can
therefore model resources as chains of thunks.

If @racket[key->thunk] does not return a procedure, then
@racket[R] will raise @racket[exn:fail:contract].

If @racket[key->thunk] applies @racket[R] in a way that forms a
circular dependency, then that application of @racket[R] will raise
@racket[exn:fail:unlike-assets:cycle]. Every application of @racket[R]
to some @racket[k] takes place in a continuation marked with key values.

The burden is on you to ensure that your keyspace consists of unique
values, and to define caching rules. A resolver will not understand if
a dependency between @racket{page?a=1&b=2} and @racket{page?b=2&a=1}
would form a cycle, because it compares unresolved dependencies to a
requested key using @racket[equal?]. In that case, @racket[(R
"page?a=1&b=2")] will not terminate if @racket[key->thunk]
recursively applies @racket[R] to @racket{page?b=2&a=1}. You can
rectify this by restricting the keyspace in your implementation, or
rewriting ambiguous keys with a surjective function.

@racketblock[
(define resolver (make-resolver ...))
(define (disambiguate k) ...)

(resolver (disambiguate k))
]

The burden is on you to define caching rules for procedures returned
by @racket[key->thunk]. The following resolver is wasteful because
it reads a file into memory for every application of @racket[R] to
a path.

@racketblock[
(make-resolver #hash() (lambda (key sys) (lambda () (file->string key))))
]

A more sensible implementation of @racket[key->thunk] would return
a procedure with caching behaviors, likely with some measure of change
detection.
}

@defstruct[exn:fail:unlike-assets:cycle ([dependency any/c] [dependents list?])]{
An error raised when a resolver encounters a cycle.

@racket[dependency] is the key of a requested value that formed a cycle.
That value will also appear in @racket[dependents].

@racket[dependents] is the list of keys used for some resolver leading
up to @racket[dependency], where the first element is the most recent
request. Formally, the first element in @racket[dependents] is
dependent on the value implied by @racket[dependency]. Beyond that,
the @racket[N]th element in @racket[dependents] depends on the
@racket[N-1]th element.
}

@defthing[current-resolver (parameter/c resolver?)]{
A global instance of a resolver used by @racket[procure]. The default
value is a resolver that raises an error asking for an implementation.
}

@defproc[(procure [key any/c]) (not/c procedure?)]{
Equivalent to @racket[((current-resolver) key)].
}

@defproc[(install-resolver [key->maybe-thunk (-> any/c (or/c #f (-> any/c)))] ...) void?]{
Imperatively replaces @racket[current-resolver] with a new resolver,
while preserving the cache.

This procedure combines several procedures into a single
@racket[key->thunk] argument for @racket[make-resolver].  If one
@racket[key->maybe-thunk] returns @racket[#f], the next one is
consulted for a procedure. If no procedure is found for all values of
@racket[key->maybe-thunk], then the combined procedure will raise an
@racket[exn:fail].
}
