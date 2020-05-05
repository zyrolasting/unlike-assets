#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    unlike-assets]]

@title{Defining Resolvers}
@defmodule[unlike-assets/resolver]

This module helps you write dynamic variants of @racket[require]. That
is, import procedures that are not limited to @racket[require]'s data
formats and conventions.  In this light, a non-Racket resource can
present itself as a Racket value.

In the context of this module, a @deftech{resolver} is a procedure
that cooperates with a surjective mapping of Racket values to
thunks. The thunks may return the latest version of a value, but are
not required to do so.

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
                                       (-> any/c (-> any/c))))]{
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

In this example, the hash tells us that @racket[(R "a")] and
@racket[(R "b")] will both return @racket[x].

You can create a new resolver based on another resolver's cache using
the following pattern:

@racketblock[
(define R+ (make-resolver (R) key->thunk))
]

Reclaiming memory entails discarding resolver references as well
as resolved value references before a garbage collection pass.

@racket[(R key)] caches @racket[(dependent key->thunk key (key->thunk
key R))], if it has not already done so. If @racket[key->thunk] does
not return a procedure, then @racket[R] will raise
@racket[exn:fail:contract]. Otherwise, it will return the cached value.

The burden is on you to ensure that your keyspace consists of unique
values, because a resolver will not understand if a dependency between
@racket{page?a=1&b=2} and @racket{page?b=2&a=1} would form a
cycle. You can rectify this by rewriting ambiguous keys with a
surjective function as shown below, or by using
@racket[current-rewriter].

@racketblock[
(define resolver (make-resolver ...))
(define (disambiguate k) ...)

(resolver (disambiguate k))
]

To repeat for emphasis: A resolver only caches thunks returned by
@racket[key->thunk], not the values those thunks return. The following
resolver is therefore wasteful because @racket[((R key))] will read
the given file into memory every time.

@racketblock[
(make-resolver #hash() (lambda (key sys) (lambda () (file->string key))))
]

A more sensible implementation of @racket[key->thunk] would return a
procedure with caching behaviors. For that,
@racketmodname[unlike-assets/resolver/extension] provides
@racket[fenced-factory] as a courtesy.
}

@defthing[current-resolver (parameter/c resolver?)]{
A global instance of a resolver used by @racket[procure/weak]. The default
value is a resolver that raises an error asking for an implementation.
}

@defthing[current-rewriter (parameter/c (-> any/c any/c))]{
A global instance of a rewrite procedure used by @racket[procure/weak]. The default
value is the identity function.
}

@defproc[(procure/weak [key any/c]) (-> any/c)]{
Equivalent to @racket[((current-resolver) ((current-rewriter) key))].

@racket[procure/weak] is useful for populating a resolver's cache
without forcing the current process to compute a value.
}

@defproc[(procure [key any/c]) any/c]{
Equivalent to @racket[(dependent (current-resolver) key ((procure/weak key)))].
}
