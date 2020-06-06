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

This document covers use of bindings from the
@racketmodname[unlike-assets] package.  Unlike Assets as a project
offers @hyperlink["https://github.com/zyrolasting/unlike-assets"]{more
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

(current-resolver
  (make-resolver
    (existing-files file-or-directory-modify-seconds
                    (search-within (this-directory/)))))]

This installs a custom resolver in the @racket[current-resolver]
parameter. It uses @racket[existing-files] to looks for files on disk,
and will resolve relative paths using @racket[search-within]. The
resolved value will be the current modification time of the file, in
seconds.


@section{The @racket[procure] Procedure}

@racket[procure] is a front end to @racket[current-resolver] that
works independently of Racket's module resolver.

Open a REPL in the directory containing @tt{resolver.rkt} and try the
following session:

@racketinput[(require unlike-assets)]
@racketinput[(require "resolver.rkt")]
@racketinput[(procure "resolver.rkt")]
@racketresult[1588629393]

The @racket[require]s work as you'd expect. After instantiating
@racket{resolver.rkt}, @racket[procure] can inspect the file that
configured it.

@italic{With your REPL still running}, add a few blank lines to
@tt{resolver.rkt}. Save the file, then evaluate @racket[(procure
"resolver.rkt")] again.

@racketinput[(procure "resolver.rkt")]
@racketresult[1588629484]

Your resolver (specifically @racket[existing-files]) captured a change
in @tt{resolver.rkt}. This aids helpful use cases, such as documents
that maintain correct relative paths to their dependencies, and
development servers that show you the latest preview of your work.


@section{Cooperating with Racket's Initialization Flow}

If you write a Racket module that depends on a particular value of
@racket[current-resolver] on instantiation, you will not be able to
load that module without raising an
@racket[exn:fail:unlike-assets:unresolved] exception.

For example, you cannot instantiate this module using any Racket
launcher because @racket[current-resolver] defaults to
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
how Racket starts, and it leverages a runtime configuration file
that's familiar to more people.

One drawback is that you need to do this for each module. You can
can introduce a @racket[nearest-u/a] at read- or expand-time
if you write your own language.

Another drawback is unexpected behavior if someone writes a
conflicting configuration that @racket[nearest-u/a] matches. You can
mitigate the risk by limiting the search to an exact file name.

@racketmod[
racket/base

(require unlike-assets
         (nearest-u/a "resolver.rkt"))

(define styles (procure "styles.css")) (code:comment "All good.")]


@section{On Replacing @racket[procure] With @racket[require]}

If you want to avoid using @racket[procure], then you probably
don't want a custom module resolver in the first place.

It's possible to make Racket's module resolver use @racket[procure],
such that you can write @racket[(require "styles.css")] and make it work.
If you only ever intend to use your project directly from source code,
then this will work fine and without too many hiccups. But if you want
to distribute executables, your troubles are just starting.

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
cache by using the following pattern:

@racketblock[
(current-resolver (make-resolver (filter-outdated ((current-resolver))) key->thunk ...))]

@racket[((current-resolver))] returns the resolver's cache, and
@racket[filter-outdated] is a hypothetical procedure that removes any
entries that no longer apply.

Since this operation discards both the original resolver and cache
values, it may be sufficient for a productive garbage collection pass.

On a related note, a resolver only caches thunks returned by a
procedure passed to @racket[make-resolver], @italic{not the values
those thunks return}. The following resolver is therefore wasteful
because every application of @racket[procure] to a given @racket[key]
will read a file into memory.

@racketblock[
(current-resolver
  (make-resolver #hash()
                 (lambda (key dependent resolver)
                   (lambda () (file->string key)))))]

A more sensible implementation would return a thunk with its own
caching pattern. Just be sure that any references to cached memory
would become inaccessible when replacing the resolver.
