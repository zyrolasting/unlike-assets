#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    unlike-assets]
                    "elements.rkt"]

@title{Model}

Humans use unspecific language to mean specific things. In an
expression like @racket[(procure "logo.svg")], what's returned? Where
does the SVG resource come from? How do we get it? If we know the
answers in advance, we can write more expressive code.

@racket{logo.svg} is an @deftech{unresolved name}. Specifically, an
unresolved name is @bold{any} Racket value that communicates what the
user wants with limited context.

Here, a @deftech{resolver} maps an @tech{unresolved name} to a
@deftech{resolved name} and a procedure that computes a
@deftech{resolved value}.  A user may compose resolvers to handle
different data formats and use cases. Once a user is satisfied with
the capabilities of a resolver, (s)he may then bind it to a
@deftech{seat}. A seat caches responses from a resolver and equips
that resolver with the means to recursively request other resources
using @tech{unresolved names}. That way, multiple resolvers can
communicate If a user installs their seat in @racket[current-seat],
then expressions like @racket[(procure "logo.svg")] will work
according to the user's expectations.

Tool authors should use @racket[make-resolver] to serve their users
under a specific configuration. @racket[make-filesystem-resolver] is an
example of a procedure that returns a tailored resolver.

@tt{unlike-assets} uses @tech/reference{continuation marks} to track
dependency relationships and detect cycles. A @tech{seat} will compute
a @deftech{dependents list} for a resolver to consider in context.  A
dependents list consists of @tech{resolved names}, such that a value
computed from the name in position @tt{N} is dependent on the value
computed from the name in position @tt{N-1}. The value named by the
first element is dependent on a value that is not yet represented in
the dependents list. A programmer should interpret this gap in the
context of @racket[exn:fail:unlike-assets:cycle] and
@racket[make-resolver].