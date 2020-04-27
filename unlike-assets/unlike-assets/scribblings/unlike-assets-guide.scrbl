#lang scribble/manual

@require[@for-label[
racket/base
racket/rerequire
unlike-assets
u/a]]

@title{Unlike Assets: Guide}
@author{Sage Gerard}

Imagine if Racket allowed this, and it did what you meant.

@racketblock[
(dynamic-rerequire "/path/to/script.js")
(dynamic-require "/path/to/script.js" 'minified)
]

This is basically what Unlike Assets (UA) does.

@racketblock[(procure "/path/to/script.js")]

More specifically, UA offers a programmable resolver that does not
have to comply with the restrictions imposed by Racket's module
resolver. The tradeoffs work towards the end of helping Racketeers
set up their own workflow for creative projects.

In that sense, UA as a project abstracts over creative frameworks.

Here I will cover configuration and use of the default resolver
without any optional packages. Once you are accustomed to programming
your own resolver, see @other-doc['(lib "unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")]


@section{Setup}
Let's start by installing the Unlike Assets package and writing a
configuration file. The name of the configuration file is up to you.

@verbatim|{
raco pkg install unlike-assets
mkdir my-project
$EDITOR my-project/resolver.rkt
}|

Write this code into a file in a fresh directory. You can call
the file whatever you'd like, but I'll call it @tt{project.rkt}
here.

@racketmod[u/a

(replace-resolver
  (file-modules show-file-info
                (search-within this-directory)))]

This sets up a custom resolver that responds to requests
for files with stat information.

@section{@racket[procure]}

The @racket[procure] function acts as a runtime variant
of @racket[require] that uses your configuration. Let's
use it to inspect the same configuration file we just
wrote. Open DrRacket or your REPL and try the following
session:

@racketinput[
(require "./project.rkt")
(procure "./project.rkt")
]
@racketresult['...]

The @racket[require] works as you'd expect, but by instantiating
@racket{./project.rkt} we installed our resolver. This changes the
behavior of @racket[procure], which we use to inspect the very file
that configured it.

The extensions that Unlike Assets provides for you tries to keep
values up to date. @italic{With your REPL still running}, edit
@tt{project.rkt} by adding a few blank lines somewhere in the
file. Then come back and evaluate @racket[(procure "./project.rkt")]
again.

@racketinput[
(procure "./project.rkt")
]
@racketresult['...]

Even though your program did not change, it captured a change
external to the program. This opens up some interesting use cases,
such as documents that maintain correct relative paths to their
dependencies, and development servers that show you the latest
preview of your work.

As it turns out, Unlike Assets ships with packages that include those
features.

@section{Development Server}

Shut down your REPL and edit @tt{project.rkt} like so:

@racketmod[u/a

(replace-resolver
  (file-modules show-file-info
                (search-within this-directory)))

(module+ main (u/a-cli))]

This installs the default command line interface, which allows you to
observe logs, start a prototype server, and export files.

In your terminal, run @litchar{racket project.rkt --server --verbose --port 9000}.

@tt{--server} starts a development server on the given @tt{--port}.
@tt{--verbose} simply prints out debug level log messages, which is
helpful for tracking requests in your resolver, and the behavior of
extensions.

The server applies @racket[procure] to the URL path in every GET request.
Using @tt{curl} or your browser, navigate to @tt{[::]:9000/project.rkt}.

Notice that unlike your REPL, you see the actual Racket code of
@tt{project.rkt}. This is one of Unlike Assets' features. The assets
produced by your resolver may integrate with different extensions in
their own way. This means that you can present a different
representation of data that is most fitting to the software component
that consumes it.

Since the server uses @racket[procure], you can edit @tt{project.rkt}
and request the file again to see changes. Try it now.

@section{Distributor}
Unlike Assets ships with a @italic{distributor}, a powerful system
that exports your assets as bytes over some port. It includes a sync
algorithm that can force several directories to reflect the current
state of your resolver, and a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{security guard}
that prevents unwanted writes to disk.

Remember that assets can present themselves differently to different
consumers. This means that you can preview files for development
purposes in your server, and save production-ready versions to disk
using the distributor.

@margin-note{Why list @tt{project.rkt} twice? Because the arguments you pass on the
CLI are @italic{preloaded}. The distributor exports data that our
resolver has already encountered. If we didn't specify the assets we
want loaded for export, then the distributor would have nothing to do.}
Run @litchar{racket project.rkt --distribute --verbose project.rkt}.
You will see the distributor prompt you with a warning that it's work
may be destructive. Heed it, and select the default option for a dry
run.  The distributor will then log what it would have done, without
modifying the contents of your disk.

Look closely at the dry run log: It would have overwritten your
@tt{project.rkt} file. Don't worry; it would have been replaced with
the same content. But the configuration we used tells Unlike Assets to
treat our sole project configuration file like an asset. Some
interesting projects might do this, but let's do something a little
easier to visualize.

Edit your configuration like so:

@racketmod[u/a

(replace-resolver
  (file-modules (distribute-to (this-directory/ "dist")
                               show-file-info)
                (search-within (this-directory/ "assets"))))

(module+ main (u/a-cli))]

@margin-note{UA has safeguards from contradictory instructions, just in case
two distributor configurations collide over the same value.}
Notice that we wrapped @racket[show-file-info] in an instruction on how to
handle file modules, rather than replace it outright. This is because
UA allows you to compose policies on how to handle assets.
@racket[distribute-to] adds an instruction for the distributor
to write resolved files to a @tt{dist} directory.

From this point on, the resolver will no longer find @tt{project.rkt}
because we told it to look in the @tt{assets} subdirectory.

Make the assets directory, and toss in a file of your choosing. I'll
call mine @tt{bunny.jpeg} because I love rabbits.

Bunny break!

img

Once you feel fuzzy, run @litchar{racket project.rkt --distribute
--verbose bunny.jpeg}. Say yes to the prompt this time.  You will see
that the bunny file has been copied to the dist directory.

As you learn more, you'll be able to configure the distributor to
do more interesting things than a vanilla copy.

You now have the beginnings of a policy that controls a module
resolver, an export tool, and a server. In five lines of code.

But we're just getting started.

@section{Dependency Relationships}

Unlike Assets shines in its ability to handle dependency
relationships. Let's try a new configuration that demonstrates what I
mean.

@racketmod[u/a

(replace-resolver
  (racket-modules (distribute-to (this-directory/ "dist")
                                 show-file-info)
                  (search-within (this-directory/ "assets"))))

(module+ main (u/a-cli))]

This resolver will dynamically load Racket modules with reload
support, in keeping with @racket[procure]'s intended purpose.

Let's write two Racket modules. Here's the source.

@section{Instrumenting a Racket Module With Your Resolver}
You'll notice that if you run one of the Racket modules in the last
section on its own, you will get an error that indicates
@racket[procure] does not know what to do. This is because
your configuration module needs to run first.

UA ships with @racket[nearest-resolver], which you can use in
@racket[require].  When used in a module, it will search for and load
neighboring files and files in parent directories for a resolver
configuration. In this example, @racket[(nearest-resolver)] expands
to the shortest relative path to a Racket module using @litchar{#lang u/a}.

@racketblock[
(require (nearest-resolver))
(define value (procure "something"))
]

Be warned that @racket[nearest-resolver] adds disk activity by
searching for a file.  It also means that this module can radically
change behavior if someone inserts a new @litchar{#lang u/a}
file. This might be desireable if you come from a world of
@tt{.gitignore} and @tt{node_modules}. But if you ask me, it's better
to use @racket[nearest-resolver] when you need it, and then remove it
afterwards.

If you want to limit the disk activity a little bit, you can
tell @racket[nearest-resolver] the name of your config file.

@racketblock[
(require (nearest-resolver "project.rkt"))
(define value (procure "something"))
]


@section{What Now?}
From here, you are free to write your own resolver.
See @other-doc['(lib "unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")]

Unlike Assets as a project contains more packages than just
@tt{unlike-assets}. This means that others can add support for new
formats like JavaScript, SVG, WEBP, SPIR-V shaders in terms of
@racket[procure]. The
@hyperlink["https://github.com/zyrolasting/unlike-assets"]{source
code} shows what I suppose you could call the "canonical" packages,
but how your resolver behaves is honestly up to you.

I hope by now you start having ideas about how to construct website
builders, video game scene graphs, and advanced preview tools using
Unlike Assets. That's what I'm going to use it for, and I've spent
close to a year polishing it's design.

If you want to see UA grow with more neat features, then please
consider
@hyperlink["https://github.com/sponsors/zyrolasting"]{sponsoring my
work}. I count on my users for support so that I can continue making
free and open source libraries for the public good.

Thank you.
