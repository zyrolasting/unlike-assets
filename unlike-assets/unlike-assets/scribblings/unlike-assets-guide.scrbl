#lang scribble/manual

@require[@for-label[racket/base]]

@title{Unlike Assets (2nd Edition)}
@author{Sage Gerard}

Imagine if Racket allowed this:

@racketblock[
(dynamic-rerequire "/path/to/script.js")
(dynamic-require "/path/to/script.js" 'minified)
]

This is basically what Unlike Assets (UA) does,
only in fewer words:

@racketblock[
(procure "/path/to/script.js" 'minified)
]

More specifically, UA offers a programmable module resolver for
non-Racket ecosystems. The packages with names matching
@tt{unlike-assets2-*} extend this module resolver to work
with other formats or protocols.

As an end-user, you can write modules like this to organize
a creative project:

@racketmod[
#lang u/a

(require unlike-assets2
         "my-asset-definitions.rkt")

(u/a (posts)
     (stylesheets)
     (cache-busters)
     (feeds))

(module+ main (u/a-cli))
]

@section{Differences from the First Edition}

@racketmodname[unlike-assets] uses a couple of graph models to manage
dependency relationships, but it leaves a lot of work for you to do to
configure it. The second edition is a refactoring centered around the
Reactive Model that includes an asynchronous module resolver, and
opt-in packages that make Racket compatible with non-Racket code and
data.

How is this an improvement? Let's put it this way: the first edition
powers the @racketmodname[polyglot] collection. The second edition
helps you write your own alternative to @racketmodname[polyglot] in
less than 100 lines of code. And we're going to do that in this guide.

Put more generally, UA as a project is meant to abstract over
creative frameworks like Pollen, Polyglot, Koyo, and others.
