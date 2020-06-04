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

This is what Unlike Assets (UA) allows you to do.

@racketblock[(hash-ref (procure "/path/to/script.js") 'minified)]

UA allows you to treat CSS, SVG, JavaScript, and other resources as if
they were software modules. In that sense, UA abstracts over creative
frameworks so that you can create your own alternatives to Polyglot,
Scribble, Frog, Universe, Pollen, etc.

I count on user support to make software like this. If this is helpful
to you then please consider
@hyperlink["https://github.com/sponsors/zyrolasting"]{sponsoring my
work}.


@section{How to Use This Guide}

This document only covers use of bindings from
@racketmodname[unlike-assets] without any optional packages. This
means we will @italic{not} cover creating a competitor to an incumbent
technology in this guide.

While this guide is meant to be a friendly introduction, UA is for
programmers that are already comfortable with Racket. I will not
explain the meaning of changes between examples that are not unique to
UA.

The guide is structured in order of increasing difficulty and
flexibility.  I'll start with clean and understandable recipes.  I'll
then add features, and strip off a layer of helpers to show you
how everything works under the hood. The guide will conclude with
a non-trivial exercise that will expose you to some nuance.

The guide's structure is meant to help you stop reading once you have
what you need. Those of you who finish the guide entirely will be able
to write add-ons to Unlike Assets, and will be able to comfortably
digest @other-doc['(lib
"unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")].


@section{Setup}

Let's start by installing the Unlike Assets package and writing a
configuration file in a new directory. The name of the configuration
file is up to you, but I'll call it @tt{project.rkt}.

@verbatim|{
raco pkg install unlike-assets
mkdir my-project
$EDITOR my-project/project.rkt
}|

@racketmod[#:file "project.rkt"
u/a

(replace-resolver
  (existing-files get-file-info
                  (search-within (this-directory/))))]

This installs a custom global resolver. It uses
@racket[existing-files] to looks for files on disk, and will resolve
relative paths using @racket[search-within]. The resolved value will
be information about an associated file.


@section{The @racket[procure] Procedure}

@racket[procure] is a runtime variant of @racket[require] that uses
your code. The configuration module we wrote installs a global
resolver that @racket[procure] uses to compute a value.

See for yourself. Open a REPL in the directory containing
@tt{project.rkt} and try the following session:

@racketinput[(require unlike-assets)]
@racketinput[(require "project.rkt")]
@racketinput[(procure "project.rkt")]
@racketblock[
'#hasheq((file-or-directory-identity . 268426781817660486314949890)
         (file-or-directory-modify-seconds . 1588629393)
         (file-or-directory-permissions . (write read))
         (file-size . 113))]

The @racket[require]s work as you'd expect. By instantiating
@racket{project.rkt}, @racket[procure] is now able to inspect
the file that configured it.

@italic{With your REPL still running}, add a few blank lines to
@tt{project.rkt}. Save the file, then evaluate @racket[(procure
"project.rkt")] again.

@racketinput[(procure "project.rkt")]
@racketblock[
'#hasheq((file-or-directory-identity . 268426800264404560024501506)
         (file-or-directory-modify-seconds . 1588629484)
         (file-or-directory-permissions . (write read))
         (file-size . 113))]

Your resolver (specifically @racket[existing-files]) captured a change
in @tt{project.rkt}. This aids helpful use cases, such as documents
that maintain correct relative paths to their dependencies, and
development servers that show you the latest preview of your work.



@section{What Now?}
From here, you are free to write your own resolver.

If you use a single resolver, I'd hope that your configuration would
look something like this:

@racketmod[#:file "project.rkt"
u/a

(require "assets.rkt")

(replace-resolver
  (essays)
  (stylesheets)
  (spreadsheets)
  (shaders))

(module+ main (u/a-cli))]


This configuration represents the envisioned experience.

If you want a web page with an image showing a SPIR-V shader applied to a
sphere, then dammit, you want a web page with an image showing a SPIR-V
shader applied to a sphere. Having a custom resolver that dances with
your dependencies makes that effort less daunting.

On that note, Unlike Assets as a project contains more packages than
just @tt{unlike-assets}. The
@hyperlink["https://github.com/zyrolasting/unlike-assets"]{source
code} shows what I suppose you could call the "canonical" packages,
but how your resolver behaves is honestly up to you. I hope that
others will add their own extensions so that you can piece together
something especially cool with little effort.

If you want to see UA grow with more neat features, then please
consider
@hyperlink["https://github.com/sponsors/zyrolasting"]{sponsoring my
work}. I count on my users for support so that I can continue making
free and open source libraries for everyone.

Thank you.


@section{Addendum: @tt{"No thunk for key: ..."}}

This error is thrown by a procedure built for you using
@racket[replace-resolver].  It means that none of the procedures you
provided returned a thunk. The default resolver will always throw this
error, which means you should not use @racket[procure] before your
resolver is installed. That could happen if you, say, start a REPL in
a Racket module that uses @racket[procure] at the top level.

UA ships with @racket[nearest-u/a], which you can use in
@racket[require]. @racket[nearest-u/a] will search neighboring
files and parent directories for a @litchar{#lang u/a} file.

@racketblock[
(require (nearest-u/a))
(define value (procure "something"))
]

If you want to limit the disk activity a little, then you can tell
@racket[nearest-u/a] the name of your config file.

@racketblock[
(require (nearest-u/a "project.rkt"))
(define value (procure "something"))
]

Be warned that modules using @racket[nearest-u/a] can behave
differently if someone inserts a new @litchar{#lang u/a} file. This
might be desireable if you come from a world of @tt{.gitignore} and
@tt{node_modules}. If not, use @racket[nearest-u/a] on an as-needed
basis.
