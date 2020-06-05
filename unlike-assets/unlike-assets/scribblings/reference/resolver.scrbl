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


@section{Emphasis: The Resolver Only Caches Thunks}
A resolver only caches thunks returned by a @racket[key->thunk]
procedure passed to @racket[make-resolver], @italic{not the values
those thunks return}. The following resolver is therefore wasteful
because @racket[((R key))] will read the given file into memory every
time.

@racketblock[
(define R (make-resolver #hash() (lambda (key resolver) (lambda () (file->string key)))))
]

A more sensible @racket[key->thunk] would return a thunk with its own
caching pattern.

@section{Cooperating with the Garbage Collector}

You can create a new resolver that behaves the same way with a reduced
cache by using the following pattern:

@racketblock[
(set! R (make-resolver (filter-outdated (R)) key->thunk ...))]

Since this operation discards both the original resolver and cache
values, it may be sufficient for a productive garbage collection pass.


@section{Cooperating with Racket's Initialization Flow}

If you write a Racket module that depends on a particular value of
@racket[current-resolver] on instantiation, you will not be able to
load that module without triggering
@racket[exn:fail:unlike-assets:unresolved].

For example, you cannot instantiate this module using any Racket
launcher because @racket[current-resolver] defaults to
@racket[null-resolver].

@racketmod[
racket/base

(require unlike-assets)

(define styles (procure "styles.css")) (code:comment "Raises error")
]

There are two ways to fix this.

The first fix uses @racket[nearest-u/a] to seek out a configuration
file that installs your resolver. Assuming that the nearest config
file handles stylesheets, this example will work because Racket
evaluates @racket[procure] at a lower phase.

@racketmod[
racket/base

(require unlike-assets
         (nearest-u/a))

(define styles (procure "styles.css")) (code:comment "All good.")
]

The benefit of this approach is that it requires no understanding of
how Racket starts, and it leverages a runtime configuration file
that's familiar to more people. The drawback is added disk activity at
compile time and the risk of unexpected behavior if someone writes a
conflicting configuration that overrides the result of
@racket[nearest-u/a].

The second fix is to using a custom Racket launcher or distributable
that loads your resolver configuration in advance of your code.  This
approach leverages canonical tools, and opens a path to integrating
your own resolver with Racket's (e.g. @racket[(require
"index.js")]). The drawback is that your programs will no longer be
compatible with the built-in executables, and you may need to change
how your resolver works to accomodate distribution concerns.

In short: Use @racket[nearest-u/a] for an easy out that lets you
use Racket's default tooling. Use custom initalization rules for
increased leverage over Racket, at the cost of more work.
