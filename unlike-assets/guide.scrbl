#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    racket/rerequire
                    kinda-ferpy
                    unlike-assets/resolver]]

@title{Unlike Assets: Resolver}
@author{Sage Gerard}

@section{Guide}

Imagine if this worked and did what you meant:

@racketblock[(begin (dynamic-rerequire "index.js")
                    (dynamic-require "index.js" 'minified))]

@racketmodname[unlike-assets/resolver] provides a shared, configurable
@deftech{resolver} that handles this kind of thing. Instead of using
@racket[require], you use @racket[procure].

@racketinput[
(procure "styles/main.css.rkt" 'css)
]
@racketresult|{*{box-sizing:border-box}h1,h2,h3{color:...}|

@racket[procure] is the module resolver for the world outside of
Racket.  It's job is to turn some vague, hand-wavy request for
something into a concrete, usful Racket value. We define how that
happens.

@tt{unlike-assets-*} packages are meant to include integrations for
other data formats so that they work with @racket[procure]. Making
Racket more useful as a coordinator of ecosystems beyond its own
is a matter of extending @racket[procure].

@subsection{Setup}

I wrote a project a while back called Polyglot. It did some cool
things, but I made it so that others would learn not to depend on
frameworks like it. To prove that point, we're going to implement
an alternative to Polyglot in 20 lines of code.

Go to your shell and create a new Racket project with a couple of
directories like so:

@verbatim|{
$ raco pkg new my-website
$ mkdir my-website/{assets,dist}
}|

Create a new @tt{cli.rkt} file wit in @tt{my-website} with this code:

@racketmod[#:file "cli.rkt"
u/a

(require unlike-assets/markdown
         unlike-assets/css
         unlike-assets/files)

(define-runtime-path assets/ "assets")
(define-runtime-path dist/ "dist")

(u/a (markdown-modules assets/ dist/)
     (css-modules assets/ dist/)
     (static-files cache-bust dist/))

(module+ main (u/a-cli))]

While you're at it, go ahead and write a Markdown file
with some goodies in it.


Save, back out, and launch the script.

@verbatim|{
$ racket my-website/cli.rkt -vd index.html
}|


@section{Here's What Happened}

You just configured a system that understands more than Racket,
and can only ever grow.

The @tt{cli.rkt} script uses the highest-level helpers available
in the @tt{unlike-assets} collection to reason about Markdown,
CSS, and static files as if they were software modules. Each
of these modules are configurable and have a life cycle of their
own.

When you ran the script, you made a request for an HTML file.
The @racket[markdown-modules] procedure was listening for that.
When it saw your request, it knew to find an associated Markdown
file and,

@section{Reference}
