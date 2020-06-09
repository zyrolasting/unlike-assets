#lang scribble/manual

@require[@for-label[
racket/base
racket/hash
racket/path
racket/port
racket/rerequire
unlike-assets
u/a]
racket/runtime-path]

@title{Unlike Assets: Guide}
@author{Sage Gerard}

Imagine if Racket allowed this, and it did what you meant.

@racketblock[
(dynamic-rerequire "/path/to/script.js")
(dynamic-require "/path/to/script.js" 'minified)
]

UA helps you write custom module resolvers for non-Racket
resources. This goes a long way towards creating your own alternatives
to Polyglot, Scribble, Frog, Universe, or Pollen.

This document covers use of bindings from the @tt{unlike-assets}
package.  Unlike Assets as a project offers
@hyperlink["https://github.com/zyrolasting/unlike-assets"]{more
packages} that extend what you will learn about here, and adds support
for different data formats.

I depend on your support to make open source software. If this is
helpful to you then please consider
@hyperlink["https://sagegerard.com/subscribe.html"]{sponsoring my
work}.


@section{Setup}

Install the @tt{unlike-assets} package and write a @tt{resolver.rkt}
file in a new directory.

@verbatim|{
raco pkg install unlike-assets
mkdir my-project
$EDITOR my-project/resolver.rkt
}|

@racketmod[#:file "resolver.rkt"
u/a

(use (make-filesystem-resolver
        #:search-directories (list (this-directory/))
        (lambda (path dependents seat)
          (file-or-directory-modify-seconds path))))]

This installs a custom resolver that looks for files on disk in the
directory where @racket{resolver.rkt} appears. The resolved value will
be the current modification time of the file, in seconds.

@section{The @racket[procure] Procedure}

Open a REPL in the directory containing @tt{resolver.rkt} and try the
following session:

@racketinput[(require unlike-assets)]
@racketinput[(require "resolver.rkt")]
@racketinput[(procure "resolver.rkt")]
@racketresult[1588629393]

The @racket[require]s work as you'd expect. After requiring
@racket{resolver.rkt}, @racket[procure] uses the resolver
we installed to return information about our file.

@italic{With your REPL still running}, add a few blank lines to
@tt{resolver.rkt}. Save the file, then evaluate @racket[(procure
"resolver.rkt")] again.

@racketinput[(procure "resolver.rkt")]
@racketresult[1588629484]

The resolver captured a change in @tt{resolver.rkt}. This aids helpful
use cases, such as documents that maintain correct relative paths to
their dependencies, and development servers that show you the latest
preview of your work.


@section{Cooperating with Racket's Initialization Flow}

@racket[procure] works independently of @racket[require], and
at a lower phase level.

If you write a Racket module that depends on a particular resolver on
instantiation, you will not be able to load that module without
raising an error.

For example, you cannot instantiate this module using any Racket
launcher the default resolver is
@racket[null-resolver]. @racket[null-resolver] resolves nothing.

@racketmod[
racket/base

(require unlike-assets)

(define styles (procure "styles.css")) (code:comment "Raises error")
]

You can fix this by launching Racket with your resolver in advance
of the rest of the program.

@verbatim|{
$ racket -t resolver.rkt -t program.rkt
}|

Alternatively, use @racket[nearest-u/a] searches for a @litchar{#lang
u/a} near the file in which it's used.  This example will work because
Racket evaluates @racket[procure] at a lower phase.

@racketmod[
racket/base

(require unlike-assets
         (nearest-u/a))

(define styles (procure "styles.css")) (code:comment "All good.")]

The benefit of this approach is that it requires no understanding of
how Racket starts, and it leverages a configuration file approach
that's familiar to more people.

The drawbacks are that you need to do this for each module, and
you can't write conflicting configuration files in the wrong place.

You can mitigate these problems by limiting the search to an exact
file name (e.g. @racket[(nearest-u/a "resolver.rkt")], and using a
different language that expands like so:


@racketmod[
document

@stylesheet[@procure{styles.css}]

This is my document. It is profound.]

@racketmod[
racket/base

(provide doc)

(require unlike-assets
         document
         (nearest-u/a "resolver.rkt"))

(stylesheet (procure "styles.css"))
(define doc '("This is my document. It is profound."))
]


@section{On Replacing @racket[procure] With @racket[require]}

If you want to avoid using @racket[procure] altogether, then you
probably don't want a custom module resolver in the first place.

It's possible to make Racket's module resolver use @racket[procure],
such that you can write @racket[(require "styles.css")] and make it
work.  If you only ever intend to use your project directly from
source code, then that's one thing. But if you want to distribute
executables, your troubles are just starting.

Do you embed a stylesheet directly in your executable, or do you
generate an equivalent Racket module in advance? Do you want to
resolve non-Racket names to Racket modules, or do you want to resolve
names to Racket modules that you promise will exist once you generate
them?

If you use @racket[procure], you have to embed non-Racket resources in
your executables. If you don't use @racket[procure], I'm personally
warm to the idea of generating Racket modules from non-Racket
resources in advance and just using Racket's module resolver without
modifications. @racket[procure] works independently of
@racket[require] precisely because you can decide how it works without
needing to solve another layer of integration problems. If my advice
counts for anything, then don't cross the streams.


@section{Cooperating with the Garbage Collector}

You can create a new resolver that behaves the same way with a reduced
cache by using the following hypothetical expression:

@racketblock[(use #:cache without-extra-stuff resolvers ...)]

Since this operation discards both the original resolver and cache
values, it may be sufficient for a productive garbage collection pass.
