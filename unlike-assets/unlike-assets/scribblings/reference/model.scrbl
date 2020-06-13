#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    unlike-assets/resolver]
                    "elements.rkt"]

@title{Model}

Humans use unspecific language to mean specific things. In an
expression like @racket[(procure "logo.svg")], what's returned? Where
does the SVG resource come from? How do we get it? This package helps
you write module resolvers to answer those questions.

@racket{logo.svg} is an @deftech{unresolved name}. Specifically, an
unresolved name is @bold{any} Racket value that communicates what the
user wants with limited context (e.g. a symbol, a string, a URL, or a
list like @racket['(project-relative "posts/page.html" href)]).

Here, a @deftech{resolver} maps an @tech{unresolved name} to a
@deftech{resolved name} and the means to compute a @deftech{resolved
value} later.  A user may compose resolvers to handle different data
formats and use cases. Once a user is satisfied with the capabilities
of a resolver, (s)he may then bind it to a @deftech{seat}. A seat
caches responses from a resolver and acts as an entry point to request
other resources by unresolved names. Once a user installs their seat
in @racket[current-seat], @racket[(procure "logo.svg")] will work.

@tech{Resolvers} use @tech/reference{continuation marks} to track
dependency relationships and detect cycles. A resolver uses a
@deftech{dependents list} when considering if a dependency cycle has
occurred. A dependents list consists of @tech{resolved names}, such
that a value computed from the name in position @tt{N} is dependent on
the value computed from the name in position @tt{N-1}. The value named
by the first element is dependent on a value that is not yet
represented in the dependents list. A programmer should interpret this
gap in the context of @racket[exn:fail:unlike-assets:cycle] or
@racket[make-resolver].
