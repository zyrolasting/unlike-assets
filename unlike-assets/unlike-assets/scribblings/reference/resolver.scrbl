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
present itself as a Racket value via @racket[procure].

In the context of this module, a @deftech{resolver} is a procedure
that cooperates with a surjective mapping of Racket values to
thunks. The thunks are meant to return the latest version
of a value.

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

@racket[(R key)] caches @racket[(key->thunk key R)], if it has not
already done so. It will then return that value. If
@racket[key->thunk] does not return a procedure, then @racket[R] will
raise @racket[exn:fail:contract].

If @racket[key->thunk] applies @racket[R] in a way that forms a
circular dependency, then that application of @racket[R] will raise
@racket[exn:fail:unlike-assets:cycle].

Every application of @racket[R] to some @racket[k] takes place in a
continuation marked with the keys of unresolved dependents. When a new
dependent is recorded, @racket[R] will log the following with the
@racket['unlike-assets] on the @racket['debug] level:

@verbatim|{
unlike-assets: dependents: '("styles.css" "index.html")
}|

In English, this means "I am now trying to resolve @tt{styles.css},
which is a dependency of the also unresolved @tt{index.html}". If you
track these messages, you can construct a dependency graph to analyze
the shape of a project. If you have a circular dependency, these
messages can help you figure out how to fix that. The dependents list
shown in the log message is attached to the logged event as data.

Dependent keys in a continuation are compared using @racket[equal?] to
detect cycles. The burden is on you to ensure that your keyspace
consists of unique values, because a resolver will not understand if a
dependency between @racket{page?a=1&b=2} and @racket{page?b=2&a=1}
would form a cycle. You can rectify this by rewriting ambiguous keys
with a surjective function as shown below, or by setting
@racket[current-rewriter].

@racketblock[
(define resolver (make-resolver ...))
(define (disambiguate k) ...)

(resolver (disambiguate k))
]

A resolver only caches thunks returned by @racket[key->thunk], not the
values those thunks return. The following resolver is wasteful because
it reads a file into memory for every application of @racket[R] to a
path.

@racketblock[
(make-resolver #hash() (lambda (key sys) (lambda () (file->string key))))
]

A more sensible implementation of @racket[key->thunk] would return
a procedure with caching behaviors, likely with some measure of change
detection. As a courtesy, @racketmodname[unlike-assets/resolver/extension]
provides @racket[fenced-factory] to aid caching.
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
without forcing the current process to compute a value. A circular
dependency will still raise @racket[exn:fail:unlike-assets:cycle].
}

@defproc[(procure [key any/c]) any/c]{
Equivalent to @racket[((procure/weak key))].
}
