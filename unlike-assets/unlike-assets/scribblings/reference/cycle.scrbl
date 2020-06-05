#lang scribble/manual

@require[@for-label[racket/base racket/format unlike-assets]]

@title{Cycle Detection}
@defmodule[unlike-assets/cycle]

This module provides continuation-based cycle detection.

@defform[(dependent scope key body ...)]{
In layman's terms, @racket[(dependent R "index.html" body ...)] means
"@racket[body] will compute a value for @racket{index.html} using
@racket[R]." If that statement occurs recursively, then that's
considered a cycle.

Specifically, @racket[dependent] raises
@racket[exn:fail:unlike-assets:cycle] if at least two equivalent
expansions appear in a dynamic extent. Two expansions are equivalent
if the @racket[scope]s and @racket[key]s of the forms are
@racket[equal?].

In the example below, the nested @racket[dependent] raises an
error. You can eliminate the error if you change the @racket[0] or
@racket["key"] in either form to some other value.


@racketblock[
(dependent 0 "key" (dependent 0 "key" (void)))
]

@racket[scope] scopes keys such that two resolvers do not conflict over
a resource accessed by the same key.

@racketblock[
(dependent resolver-A "index.html"
  (dependent resolver-B "index.html" (code:comment "No error")
    (dependent resolver-A "about.html"
      (dependent resolver-A "index.html" ...)))) (code:comment "Error")
]

As a side-effect, @racket[dependent] will log something like following
with the @racket['unlike-assets] topic on the @racket['debug] level:

@verbatim|{
unlike-assets: dependents: <scope> '("styles.css" "index.html")
}|

@tt{<scope>} is @racket[(~v scope)], and the following list means the
same as the @racket[dependents] field in
@racket[exn:fail:unlike-assets:cycle].

@racket[dependent] attaches @racket[(list scope dependents)] to each
log message so that you can construct dependency graphs for analysis.
}

@defstruct[exn:fail:unlike-assets:cycle ([scope any/c] [dependency any/c] [dependents list?])]{
An error raised when two equivalent @racket[dependent] forms appear in
a dynamic extent.

@racket[scope] is an @racket[eq?]-comparable value that is presumably
responsible for producing @racket[dependency] and all
@racket[dependents].  The error implies that @racket[scope] has been
asked to produce @racket[dependency] when @racket[dependency] is
already a member of @racket[dependents].

@racket[dependency] is the key of a requested value that formed a cycle.

@racket[dependents] is the value of @racket[(get-dependents scope)] at
the moment of error. Each dependent leads up to @racket[dependency],
where the first element is the most recent request. Formally, the
first element in @racket[dependents] is dependent on the value implied
by @racket[dependency] via @racket[scope]. Beyond that, the
@racket[N]th element in @racket[dependents] depends on the
@racket[N-1]th element.
}

@defproc[(get-dependents [scope any/c]) list?]{
Returns a list of keys considered unresolved by @racket[scope].
}

@defproc[(get-first-dependent [scope any/c]) any/c]{
Returns @racket[(car (get-dependents scope))], or @racket[#f] if there are no dependents.
}
