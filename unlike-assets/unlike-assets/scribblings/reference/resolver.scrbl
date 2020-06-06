#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    unlike-assets]]

@title{Defining Custom Module Resolvers}

Unlike Assets helps you write @deftech{resolvers}, which are variants
of @racket[require] that work in an expression context (e.g.
@racket[(resolve "styles.css")]). A resolver adds caching logic and
cycle detection, but does not mandate any use of I/O.

@section{API Reference}
@defmodule[unlike-assets/resolver]

@defproc[(resolver? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] was returned by @racket[make-resolver].
}

@defproc[(make-resolver [known (hash/c procedure? (non-empty-listof string?))]
                        [#:rewrite-key rewrite-key (-> any/c any/c) values]
                        [key->thunk (-> any/c resolver? (or/c #f (-> any/c)))] ...)
                        (and/c resolver?
                               (case-> (-> (hash/c procedure?
                                                   (non-empty-listof string?)
                                                   #:immutable #t))
                                       (-> any/c (-> any/c))))]{
Returns a procedure @racket[R] that encapsulates a mutable cache based
on @racket[known].

@racket[(R)] returns a @racket[hasheq] hash representing the
resolver's cache. The cache's keys are elements of the union of all
@racket[key->thunk] codomains. The cache's values are lists of
elements of @racket[R]'s domain. In the below example, the hash claims
that @racket[(R "a")] and @racket[(R "b")] will both return
@racket[x].

@racketblock[
(R "a") (R "b") (R "c")
(R) (code:comment "#hash((#<procedure:x> . '(\"a\" \"b\")) (#<procedure:y> . '(\"c\")))")
]

@racket[(R key)] returns @racket[(make-thunk (rewrite-key key) R)],
where @racket[make-thunk] is the first @racket[key->thunk] that
returns a thunk. The thunk is cached, if it wasn't already.

Every @racket[key->thunk] may instead return @racket[#f] to indicate
that it does not produce a thunk for a given key. If every
@racket[key->thunk] returns @racket[#f], then @racket[(R key)] will
raise @racket[exn:fail:unlike-assets:unresolved].

By default, a resolver will not understand if a dependency between
@racket{page?a=1&b=2} and @racket{page?b=2&a=1} would form a
dependency cycle. Use @racket[rewrite-key] to define a unique
keyspace.

@racketblock[
(define resolver (make-resolver #:rewrite-key alphabetize-query-string ...))
]

}

@defstruct*[exn:fail:unlike-assets:unresolved ([key any/c])]{
An error raised when no thunk could be produced for a given @racket[key].
}

@defthing[null-resolver resolver?]{
A resolver that raises @racket[exn:fail:unlike-assets:unresolved] for
all keys.
}

@defthing[current-resolver (parameter/c resolver?) #:value null-resolver]{
A shared resolver.
}

@defproc[(procure [key any/c]) any/c]{
Equivalent to @racket[(((current-resolver) key))].

In plain language, this populates the current resolver's cache with a
thunk and immediately attempts to compute a value with that thunk. The
entire process is monitored with cycle detection.
}
