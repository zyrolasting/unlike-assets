#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    unlike-assets]]

@title{Writing Resolvers}

@defmodule[unlike-assets/resolver]

@deftech{Resolvers} are variants of @racket[require] that work in an
expression context (e.g. @racket[(resolve "styles.css")]). A resolver
adds caching logic and cycle detection, but does not mandate any use
of I/O.

@defproc[(resolver? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is an element of @racket[make-resolver]'s codomain.
}

@defproc[(make-resolver [initial-cache (hash/c procedure? (non-empty-listof string?))]
                        [#:rewrite-key rewrite-key (-> any/c list? any/c) (lambda (key dependents) key)]
                        [key->thunk (-> any/c list? resolver? (or/c #f (-> any/c)))] ...)
                        (and/c resolver?
                               (case-> (-> (hash/c procedure?
                                                   (non-empty-listof string?)
                                                   #:immutable #t))
                                       (-> any/c (-> any/c))))]{
Returns a procedure @racket[R] that represents a module resolver with
a mutable cache.

@racket[(R)] returns a @racket[hasheq] hash representing a cache. It
is not a reference to the actual cache. The hash's keys are elements
of the union of all @racket[key->thunk] codomains. The hash's values
are lists of elements of @racket[R]'s domain. In the below example,
the hash claims that @racket[(R "a")] and @racket[(R "b")] will both
return @racket[x].

@racketblock[
(R "a") (R "b") (R "c")
(R) (code:comment "#hash((#<procedure:x> . '(\"a\" \"b\")) (#<procedure:y> . '(\"c\")))")
]

@racket[(R key)] caches and returns @racket[(make-thunk (rewrite-key
key dependents) dependents R)], where:

@itemlist[

@item{
@racket[make-thunk] is the first @racket[key->thunk] that returns a
thunk.  If every @racket[key->thunk] returns
@racket[#f], then @racket[(R key)] will raise
@racket[exn:fail:unlike-assets:unresolved].

A thunk returned by @racket[make-thunk] can recursively apply
@racket[R] to request resources. Dependency cycles will raise
@racket[exn:fail:unlike-assets:cycle].
}


@item{
@racket[dependents] is a list of rewritten keys for
dependent resources.  If @racket{index.html} depends on
@racket{styles.css} and @racket{styles.css} depends on
@racket{font.ttf}, then @racket[dependents] will equal
@racket['("styles.css" "index.html")] when @racket[(rewrite-key key dependents)] is
@racket{font.ttf}.
}

@item{
@racket[(rewrite-key key dependents)] is assumed to be the unambiguous
name of a resource.  By default, a resolver will not understand if a
dependency between @racket{page?a=1&b=2} and @racket{page?b=2&a=1}
would form a dependency cycle, so @racket[rewrite-key] can create a
restricted keyspace.

@racketblock[
(define resolver (make-resolver #:rewrite-key normalize-url ...))
]
}

]

}

@defstruct*[exn:fail:unlike-assets:unresolved ([key any/c])]{
An error raised when no thunk could be produced for a given @racket[key].
}

@defstruct*[exn:fail:unlike-assets:cycle ([scope procedure?] [dependency any/c] [dependents any/c])]{
An error raised when a @tech{resolver} or @racket[key->thunk]
procedure encounted a circular dependency.

@racket[exn:fail:unlike-assets:cycle-scope] is a procedure that would
never terminate if it weren't for this exception.

@racket[exn:fail:unlike-assets:cycle-dependency] is the the name of a
resource that is already a dependent, and therefore cannot be
resolved. It is equal to @racket[(rewrite-key key dependents)] in the
context of
@racket[make-resolver]. @racket[exn:fail:unlike-assets:cycle-dependents]
is equal to the value of @racket[dependents] in the same context.

}


@defthing[null-resolver resolver?]{
A resolver that raises @racket[exn:fail:unlike-assets:unresolved] for
all keys.
}

@defthing[current-resolver (parameter/c resolver?) #:value null-resolver]{
A resolver, shared globally.
}

@defproc[(procure [key any/c]) any/c]{
Equivalent to @racket[(((current-resolver) key))].
}
