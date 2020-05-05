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

On that note, Unlike Assets ships with a development server.

@section{Development Server}
Shut down your REPL and add @racket[(module+ main (u/a-cli))] to
@tt{project.rkt}. This installs the default command line interface,
which allows you to observe logs, start a prototype server, and export
files.

In your terminal, run @litchar{racket project.rkt serve --port 8080}
to start a development server on port 8080. The server applies
@racket[procure] to the URL path in every request. Using @tt{curl} or
your browser, navigate to @tt{[::]:8080/project.rkt}.

@verbatim|{
[sage@localhost ~]$ curl '[::]:8080/project.rkt'
'#hasheq((file-or-directory-identity . 268426818711148633734053122)
         (file-or-directory-modify-seconds . 1588629532)
         (file-or-directory-permissions . (write read))
         (file-size . 139))
[sage@localhost ~]$
}|

Since the server cooperates with your resolver, you will
still observe changes when you edit @tt{project.rkt}.

While the interaction looks the same, they are not equivalent. The
server uses an HTTP response derived from your configuration.
We'll see why soon.


@section{Distributor}

In this section I will cover the @deftech{distributor}, which is just
a file export tool that works with resolvers.

We're going to use an entirely different configuration for a moment.
Write this code in @tt{distro.rkt}

@racketmod[#:file "distro.rkt"
u/a

(require racket/path
         racket/port)

(define assets/ (this-directory/ "assets"))

(define (on-file input-path)
  (distributable (this-directory/ "dist" (find-relative-path assets/ input-path))
                   (lambda (to-output-file)
                     (call-with-input-file input-path
                       (lambda (from-input-file)
                         (copy-port from-input-file to-output-file))))))

(replace-resolver
  (existing-files on-file
                  (search-within assets/)))

(module+ main (u/a-cli))]

I'll show you what this does. Run @litchar{mkdir assets} in
@tt{project.rkt}'s directory and toss in this picture of Toffee, my
Holland Lop, as @tt{toffee.jpg}. If the image doesn't load, substitute
another image that makes you smile.

@(define-runtime-path toffee "toffee.jpg")
@image[toffee]

Now, run @litchar{racket project.rkt distribute project.rkt}.  By
default, the distributor performs a dry run. It logs what it would
have done without modifying the contents of your disk. You should see
an entry showing that @tt{toffee.jpg} will be copied to the @tt{dist}
directory.

Run the command again with the @litchar{--no-dry-run} option to commit
the changes to disk.

@verbatim|{
racket distro.rkt distribute --no-dry-run toffee.jpg
}|

Toffee has multiplied, making the world a warmer and fuzzier place.

Copying files is not particularly interesting, though. You can modify
this configuration to resize, crop, or otherwise transform the image
before it hits the disk.

What's more important to note is that we now have two subsystems
in the mix: the server and the distributor.

Start the server using @tt{racket distro.rkt serve} and @tt{GET
/toffee.jpg}. In a browser, you'll be prompted to download the image.

That's not quite the same behavior. The server has it's own defaults
for handling resolved data. If you're anything like me, you probably
want more control and transparency than this. In the next section,
we will coordinate the two more effectively.

@section{Using the Distributor and Server}

This configuration is a little heavier. It uses the same distribution
rules, but also ropes in rules for serving resolved values.

@racketmod[#:file "project.rkt"
u/a

(require racket/hash
         racket/path
         racket/port)

(define assets/ (this-directory/ "assets"))

(define (make-distributable input-path)
  (distributable (this-directory/ "dist" (find-relative-path assets/ input-path))
                   (lambda (to-output-file)
                     (call-with-input-file input-path
                       (lambda (from-input-file)
                         (copy-port from-input-file to-output-file))))))

(define (make-serveable path)
  (serveable (lambda (req)
               (response/output #:code 200
                 #:mime-type #"text/plain; charset=utf-8"
                 (lambda (to-client)
                   (display (get-file-info path) to-client))))))

(define (on-file path)
  (hash-union (make-serveable path)
              (make-distributable path)))

(replace-resolver
  (existing-files on-file
                  (search-within assets/)))

(module+ main (u/a-cli))]

When you serve with this configuration, a @tt{GET} response will be
the output of @racket[get-file-info] for some file. The distributor
works the same as it did before. @bold{This shows that you can
represent an asset one way for a server, and another way when
exporting files.} In general, it means that one resolver can entertain
many domains.

The @racket[on-file] procedure decides what value represents a
(possibly changed) file located at some @racket[path]. Notice that it
glues together two hashes made using @tech[#:doc '(lib
"hash-partition/scribblings/hash-partition.scrbl")]{hash partitions}.
It's harder to accidentally cause collisions this way. Even so, we'll
cover a different approach in @secref{exercise} if this causes you
concern.

There's nothing new or magical going on. All we're doing is making
sure different components get the data they expect. We are reasoning
about resources in terms of @italic{how they are used}, which is
a more useful view when supporting different domains.

We've been accepting a lot of help from U/A so far. In the next
section, we'll review the same configuration with the hood up.

@section[#:tag "training-wheels-off"]{Removing Helpers}

In this section I discuss a refactor of the configuration we built so
that it doesn't use @racket[existing-files], @racket[search-within],
or the @racketmodname[u/a] language. The result is more information-dense, but it
is an example of how to write the same code as a direct extension of
the resolver. This shows how you can apply knowledge from
@other-doc['(lib
"unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")]

@racketmod[#:file "project.rkt"
racket/base

(require racket/hash
         racket/path
         racket/port
         unlike-assets)

(define assets/ (this-directory/ "assets"))

(define (make-distributable input-path)
  (distributable (this-directory/ "dist" (find-relative-path assets/ input-path))
                   (lambda (to-output-file)
                     (call-with-input-file input-path
                       (lambda (from-input-file)
                         (copy-port from-input-file to-output-file))))))

(define (make-serveable path)
  (serveable (lambda (req)
               (response/output #:code 200
                 #:mime-type #"text/plain; charset=utf-8"
                 (lambda (to-client)
                   (display (get-file-info path) to-client))))))

(define (on-file path)
  (hash-union
    (make-serveable path)
    (make-distributable path)))

(define (resolve-file key resolver)
  (and (or (path-string? key) (path-for-some-system? key))
       (not (complete-path? key))
       (let ([path (build-path assets/ key)])
         (and (file-exists? path))
              (fenced-factory (file-or-directory-modify-seconds path)
                              (on-file path)))))

(replace-resolver resolve-file)

(module+ main (u/a-cli))]

Thankfully, it's not terribly different. Let's cover the changes.

The switch to @racketmodname[racket/base] only means that we need to
@racket[(require unlike-assets)]. This file also loses the ability to
work with @racket[nearest-u/a], but that's not a problem here.

I added a @racket[resolve-file] procedure to pass to
@racket[replace-resolver]. Notice that the @tt{assets} directory is
now mentioned there.

In the context of this configuration, calling @racket[(procure
"something.txt")], means calling @racket[(resolve-file "something.txt"
resolver)]. The @racket[resolver] argument will always be the same
resolver that's currently trying to find the value of
@racket["something.txt"].  You can apply it recursively to have that
asset depend on others. Calling @racket[(resolver key)] in the body of
@racket[resolve-file] will make the resolver complain about a circular
dependency.

@racket[resolve-file] must either return a thunk, or @racket[#f].

@itemlist[

@item{If @racket[resolve-file] returns @racket[#f], the next procedure
passed to @racket[replace-resolver] will be tried, if any exists. One
does not exist in this case, so an error would be raised complaining
about how a requested asset wasn't found. I personally use
@racket[or], @racket[and], and @racket[let] to leverage
short-circuiting and capture everything in one expression, but you
don't have to do so if the indentation drives you crazy.}

@item{If @racket[resolve-file] returns a thunk, that thunk will be cached by
the resolver. So when someone asks for the same key twice, the same
thunk will be called to compute a value. This can be a little harder
to grasp, so think of it this way: The @italic{exact} procedure
returned by @racket[(resolve-file "something.txt" resolver)] will
always be used to return the value of @racket[(procure
"something.txt")]. Once you understand more about how resolvers
work, you'll understand how to change their cache so that you
can create a new thunk when it matters.}

]

All that business about a resolver's cache brings me to
@racket[fenced-factory], which is a macro provided by UA.
@racket[fenced-factory] makes a thunk with its own cache. Every time
you call that thunk, it uses the first expression value to check for
change over time. If there is a change, it evaluates the
second expression to cache a new value. This is why you observe
@racket[procure] returning up-to-date values in our examples.

You do not have to use @racket[fenced-factory]. But you must return a
thunk to @italic{permanently} associate with some @racket[key]. That
means you can disable responses to change by avoiding
@racket[fenced-factory] altogether and returning a thunk that always
returns the same value.

If you understand how this code works, then you are ready for a pop quiz.


@section[#:tag "exercise"]{Exercise: Multiple Resolvers}

You should now have an idea of how UA's pieces fit together. This
section is left as an exercise for you to solve one last design
problem using @other-doc['(lib
"unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")].

In all of our examples, the server and distributor were each accepting
data from the same global resolver. This made us smash the data they
needed into a single value. This can be convenient, but it forces
several domains to meet in one place.

@racketblock[
(hash-union (serveable ...)
            (serveable2 ...)
            (exportable ...)
            (scannable ...)
            (make-uppable ...)
            (markable ...)
            ...)]

Thankfully, a resolver is just a kind of procedure. @racket[procure]
is nothing more than a shortcut to calling the resolver in the
@racket[current-resolver] parameter.

Write a configuration where the server and distributor each have their
own resolvers.  You will create three procedures: One maps complete
paths to @racket[distributable]s, one maps complete paths to
@racket[serveable]s, and one maps relative path strings to complete
paths.

Decide for yourself which of these procedures should be resolvers.
Modify your configuration such that your resolvers cooperate.
Finally, change the call to @racket[u/a-cli] to start the server and
distributor using different resolvers.


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
