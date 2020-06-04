#lang scribble/manual

@require[@for-label[racket/base racket/format unlike-assets]]

@title{Cycle Detection}
@defmodule[unlike-assets/cycle]

This module provides continuation-based cycle detection.

@defform[(dependent site key body ...)]{
In layman's terms, @racket[(dependent R "index.html" body ...)] means
"@racket[body] will compute a value for @racket{index.html} using
@racket[R]." If that statement occurs recursively, then that's
considered a cycle.

Specifically, @racket[dependent] raises
@racket[exn:fail:unlike-assets:cycle] if at least two equivalent
expansions appear in a dynamic extent.  Two expansions are equivalent
if the @racket[site]s of the forms are @racket[eq?] and the
@racket[key]s of the forms are @racket[equal?].

This is the simplest example of such an error.

@racketblock[
(dependent 0 "key" (dependent 0 "key" (void)))
]

Here, the nested @racket[dependent] raises the error. There is no
error if you change the @racket[0] or @racket["key"] in either form to
some other value. In context, @racket[site] is meant to add room for
multiple @tech{resolvers} that share the same keyspace.

@racketblock[
(dependent resolver-A "index.html"
  (dependent resolver-B "index.html" (code:comment "No error")
    (dependent resolver-A "about.html"
      (dependent resolver-A "index.html" ...)))) (code:comment "Error")
]

As a side-effect, @racket[dependent] will log something like following
with the @racket['unlike-assets] topic on the @racket['debug] level:

@verbatim|{
unlike-assets: dependents: <site> '("styles.css" "index.html")
}|

@tt{<site>} is @racket[(~v site)], and the following list means the
same as the @racket[dependents] field in
@racket[exn:fail:unlike-assets:cycle].

@racket[dependent] attaches @racket[(list site dependents)] to each
log message so that you can construct dependency graphs for analysis.
}

@defstruct[exn:fail:unlike-assets:cycle ([site any/c] [dependency any/c] [dependents list?])]{
An error raised when two equivalent @racket[dependent] forms appear in
a dynamic extent.

@racket[site] is an @racket[eq?]-comparable value that is presumably
responsible for producing @racket[dependency] and all
@racket[dependents].  The error implies that @racket[site] has been
asked to produce @racket[dependency] when @racket[dependency] is
already a member of @racket[dependents].

@racket[dependency] is the key of a requested value that formed a cycle.

@racket[dependents] is the value of @racket[(get-dependents site)] at
the moment of error. Each dependent leads up to @racket[dependency],
where the first element is the most recent request. Formally, the
first element in @racket[dependents] is dependent on the value implied
by @racket[dependency] via @racket[site]. Beyond that, the
@racket[N]th element in @racket[dependents] depends on the
@racket[N-1]th element.
}

@defproc[(get-dependents [site any/c]) list?]{
Returns a list of keys considered unresolved by @racket[site].
}

@defproc[(get-first-dependent [site any/c]) any/c]{
Returns @racket[(car (get-dependents site))], or @racket[#f] if there are no dependents.
}
