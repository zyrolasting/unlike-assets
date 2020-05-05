#lang scribble/manual

@require[@for-label[
racket/base
racket/rerequire
unlike-assets
unlike-assets/resolver/file
u/a]]

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


@section{Who is This For?}

You are in UA's target audience if you believe that frameworks are not
long-term solutions to your problems. You are NOT in the target
audience if you want to use someone else's workflow to do your work.


@section{How to Read this Guide}

I cover configuration and use of the default resolver without any optional packages.

In every section leading up to @secref{training-wheels-off} I present
meaningful and clear configurations that do what they say. I'll also
discuss a development server and a file export tool.

In @secref{training-wheels-off}, I replace the most developed
configuration in the guide with one that illustrates inner
workings. If you understand that section, then you are capable of
programming a resolver with @other-doc['(lib
"unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")].

Once you are accustomed to programming your own resolver, you can
expand to other @tt{unlike-*} packages and install them with in your
own resolver.


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

This is a custom resolver. It uses @racket[existing-files] to looks
for files on disk, and will resolve relative paths using
@racket[search-within]. The resolved value will be information about
the file.


@section{The @racket[procure] Procedure}

@racket[procure] acts as a configurable, runtime variant of
@racket[require].  @racket[replace-resolver] swaps out the resolver
implementation that @racket[procure] uses to compute a value.

Open a REPL in the directory containing @tt{project.rkt} and try the
following session:

@racketinput[(require unlike-assets)]
@racketinput[(require "project.rkt")]
@racketinput[(procure "project.rkt")]
@racketresult[
'#hasheq((file-or-directory-identity . 268426781817660486314949890)
         (file-or-directory-modify-seconds . 1588629393)
         (file-or-directory-permissions . (write read))
         (file-size . 113))]

The @racket[require]s work as you'd expect. By instantiating
@racket{project.rkt} we installed our resolver. This changes the
behavior of @racket[procure]. We then use @racket[procure] to inspect
the very file that configured it.

The extensions that Unlike Assets provides for you tries to keep
values up to date. @italic{With your REPL still running}, edit
@tt{project.rkt} by adding a few blank lines somewhere in the
file. Then come back and evaluate @racket[(procure "./project.rkt")]
again.

@racketinput[(procure "./project.rkt")]
@racketresult[
'#hasheq((file-or-directory-identity . 268426800264404560024501506)
         (file-or-directory-modify-seconds . 1588629484)
         (file-or-directory-permissions . (write read))
         (file-size . 113))]

Even though your program did not change, the resolver captured a
change in @tt{project.rkt}. This aids helpful use cases, such as
documents that maintain correct relative paths to their dependencies,
and development servers that show you the latest preview of your work.

Unlike Assets ships with a development server and file distribution
tools that don't care about how your resolver works. We'll cover those now.

@section{Development Server}
Shut down your REPL and edit @tt{project.rkt} like so:

@racketmod[#:file "project.rkt"
u/a

(replace-resolver
  (existing-files get-file-info
                  (search-within (this-directory/))))

(module+ main (u/a-cli))]

This installs the default command line interface, which allows you to
observe logs, start a prototype server, and export files.

In your terminal, run @litchar{racket project.rkt --serve} to start a
development server on the default port 8080. The server applies
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

Since the server cooperates with your resolver, the same
responsiveness to change applies. Edit @tt{project.rkt} and request
the file again to see changes.

While the interaction looks the same, they are not equivalent. The
server uses an HTTP response derived from your configuration. You can
use @italic{a different representation} for the server. To understand
how, we'll need to introduce a different system.


@section{Serving and Distributing}

Let's evolve a bit by adding file exports and a folder for actual
creative assets.

@subsection{Creating an Assets Directory}
To start, change the call to @racket[existing-files]
such that it will to look in an @tt{assets} subdirectory.

@racketblock[
(replace-resolver
  (existing-files serve-file/distribute-stats
                  (search-within (this-directory/ "assets"))))]

That means from this point on, @racket[(procure "project.rkt")] will
no longer resolve to that file.

Next, make the assets directory and toss in this image of sheer joy.
If the image doesn't load, substitute your own.

...

Unlike Assets ships with a @italic{distributor}. It exports
assets as files, and can sync several directories to reflect the
current state of your resolver. It comes with a @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{security guard} that
prevents unwanted writes to disk.

In the last section I mentioned that you can present data differently
to the server. The same goes for the distributor. This means that you
can preview files for development purposes in your server, and save
production-ready versions to disk using the distributor. I'll show you
how to do this before we export our first file.

We'll start by replacing the first argument of @racket[existing-files]
with our own procedure.

@racketblock[
(require racket/hash)

(define (on-file path)
  (hash-union (make-serveable path)
              (make-distributable path)))

(replace-resolver
  (existing-files serve-file/distribute-stats
                  (search-within (this-directory/ "assets"))))]

The @racket[on-file] procedure decides what value represents a
(possibly changed) file located at some @racket[path]. Notice that it
glues together two hashes using @racket[hash-union], which should make
astute readers sweat a bit. Don't fret: UA leverages uninterned
symbols for keys, namely through @tech[#:doc '(lib
"hash-partition/scribblings/hash-partition.scrbl")]{hash partitions}.
It's harder to accidentally cause collisions with these keys. Unless
you are setting out to cause a collision, don't worry about data
mixing in unacceptable ways.

Let's say we want to save our assets as copies of the input
file, and serve the output of @racket[get-file-info].

We'll start with the server. Use @racket[serveable] to tell the development
server how to serve some value.

@racketblock[
(define (make-serveable path)
  (serveable (lambda (req)
               (response/output #:code 200
                 #:mime-type #"text/plain; charset=utf-8"
                 (lambda (to-client)
                   (print (get-file-info path) to-client))))))]

Then use @racket[distributable] to define how a value is sent to
disk. It takes a path to a file you intend to write, and a procedure
used to write bytes to that file. Think of it as freeze-dried
arguments to @racket[call-with-output-file].

@racketblock[
(require racket/port)
(define (make-distributable input-path)
  (distributable (this-directory/ "dist" input-path)
                 (lambda (to-output-file)
                   (call-with-input-file input-path
                     (lambda (from-input-file)
                       (copy-port from-input-file to-output-file))))))]


With that, we have the full configuration.

@racketmod[#:file "project.rkt"
u/a

(require racket/hash
         racket/port)

(define (make-distributable input-path)
    (distributable (this-directory/ "dist" input-path)
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

(define (serve-file/distribute-stats path)
  (hash-union
    (make-serveable path)
    (make-distributable path)))

(replace-resolver
  (existing-files serve-file/distribute-stats
                  (search-within (this-directory/ "assets"))))

(module+ main (u/a-cli))]

Let's see if it works. Fire up the server again. If you
GET @tt{/toffee.jpeg}, you'll see information about my
bunny's picture.

Now we'll use the distributor. Run @litchar{racket project.rkt
--distribute project.rkt}.  By default, the distributor
performs a dry run. It logs what it would have done without modifying
the contents of your disk.

If the log looks okay, then add the @litchar{--no-dry-run} option
to commit the changes to disk.

@verbatim|{
racket project.rkt --no-dry-run --distribute bunny.jpeg
}|

This is the short form, if your hands are sore: @litchar{racket
project.rkt -nd bunny.jpeg}

You will see that the bunny picture has been copied to the @tt{dist}
subdirectory.

In this section we expanded our configuration to support exporting
files, while serving different representations of those files. We've
also adjusted the project to seek input in one directory, only to
spit out output in a different directory.

Take five. You've earned a break.

@section[#:tag "training-wheels-off"]{Optional Reading: Take the Traning Wheels Off}

In this section I discuss a refactor of the configuration we built so
that it doesn't use @racket[existing-files] and
@racket[search-within]. It's more information-dense, but
it will show how you can apply knowledge from @other-doc['(lib
"unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")].

I won't explain every part of the following code. We're getting to the
point where you need to think about what's happening.

@margin-note{I understand if indentation with @racket[and], @racket[or], and
@racket[let] drives you crazy. I personally don't mind using it
to handle returning @racket[#f] in some cases.}
@racketmod[#:file "project.rkt"
u/a

(require racket/hash
         racket/port)

(define (make-distributable input-path)
    (distributable (this-directory/ "dist" input-path)
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

(define (serve-file/distribute-stats path)
  (hash-union
    (make-serveable path)
    (make-distributable path)))

(define (resolve-file key resolver)
  (and (or (path-string? key) (path-for-some-system? key))
       (not (complete-path? key))
       (let ([path (build-path (this-directory/ "assets") key)])
         (and (file-exists? path))
              (fenced-factory (file-or-directory-modify-seconds path)
                              (serve-file/distribute-stats path)))))

(replace-resolver resolve-file)

(module+ main (u/a-cli))]

I added a @racket[resolve-file] procedure to pass to
@racket[replace-resolver]. Notice that the @tt{assets} and @tt{dist}
directories are now defined in different places.

In the context of this configuration, calling @racket[(procure
"something.txt")], means calling @racket[(resolve-file "something.txt"
resolver)]. The @racket[resolver] argument will always be the same
resolver that's currently trying to find the value of
@racket["something.txt"].  You can apply it recursively to have that
asset depend on others. Calling @racket[(resolver "something.txt")]
will make the resolver raise an error complaining about a circular
dependency.

@racket[resolve-file] must either return a thunk, or @racket[#f].

If @racket[resolve-file] returns @racket[#f], the next procedure
passed to @racket[replace-resolver] will be tried, if any exists. One
does not exist in this case, so an error would be raised complaining
about how a requested asset wasn't found.

If @racket[resolve-file] returns a thunk, that thunk will be cached by
the resolver. So when someone asks for the same key twice, the same
thunk will be called to compute a value.

I use @racket[fenced-factory] to make a thunk with its own
cache. Every time you call that thunk, it uses the first expression to
decide if it should evaluate the second. This is how a resolver can be
made to deliver the latest value: The same thunk will check for a
cache hit, then recompute the value if necessary.

You do not have to use @racket[fenced-factory]. But you must return a
thunk to @italic{permanently} associate with some @racket[key].

@section{What Now?}
From here, you are free to write your own resolver.
See @other-doc['(lib "unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")]

Ideally, your configuration would look like this:

@racketmod[#:file "project.rkt"
u/a

(require "my-stuff.rkt")

(replace-resolver
  (documents)
  (css)
  (spreadsheets)
  (shaders))

(module+ main (u/a-cli))]


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
free and open source libraries for everyone.

Thank you.

@section{Addendum: @tt{"No thunk for key: ..."}}  You'll likely
encounter this error. It means that the resolver backing
@racket[procure] could not resolve your request. The default resolver
will always throw this error, which means you should not use
@racket[procure] before your resolver is installed. That could happen
if you, say, start a REPL in a Racket module that uses @racket[procure]
at the top level.

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


@section{Addendum: Log Messages}

The default CLI supports a @tt{--verbose} option that will forward
debug messages to STDOUT. UA logs only one message to the
@racket['unlike-assets] topic on the @racket['debug] level:

@verbatim|{
unlike-assets: dependents: '("styles.css" "index.html")
}|

In English, this means "I am now trying to resolve @tt{styles.css},
which is a dependency of the also unresolved @tt{index.html}". If you
track these messages, you can construct a dependency graph to analyze
the shape of a project. If you have a circular dependency, these
messages can help you figure out how to fix that.