#lang scribble/manual

@require[@for-label[
racket/base
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

This document only covers basic use without any optional packages.

In every section leading up to @secref{training-wheels-off} I present
meaningful and clear configurations that do what they say. I'll also
discuss a development server and a file export tool.

In @secref{training-wheels-off}, I replace the most developed
configuration in the guide with one that illustrates inner
workings. If you understand that section, then you are capable of
programming a resolver with @other-doc['(lib
"unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")].

Once you are accustomed to programming your own resolver, you can
install other @tt{unlike-*} packages.


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
an associated file.


@section{The @racket[procure] Procedure}

@racket[procure] acts as a configurable, runtime variant of
@racket[require].  @racket[replace-resolver] swaps out the resolver
implementation that @racket[procure] uses to compute a value.

Open a REPL in the directory containing @tt{project.rkt} and try the
following session:

@racketinput[(require unlike-assets)]
@racketinput[(require "project.rkt")]
@racketinput[(procure "project.rkt")]
@racketblock[
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
@racketblock[
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
server uses an HTTP response derived from your configuration.
We'll see why in the next section.


@section{File Distribution}

Change the call to @racket[existing-files] such that it will look
in an @tt{assets} subdirectory.

@racketblock[
(define assets/ (this-directory/ "assets"))

(replace-resolver
  (existing-files on-file
                  (search-within assets/)))]

From this point on, @racket[(procure "project.rkt")] will no longer
resolve to that file.

Next, make that assets directory and toss in this picture of Toffee,
my Holland Lop, as @tt{toffee.jpg}. If the image doesn't load,
substitute another image that makes you smile.

@(define-runtime-path toffee "toffee.jpg")
@image[toffee]

Unlike Assets ships with a @italic{distributor}. It exports
assets as files, and can sync several directories to reflect the
current state of your resolver. It comes with a @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{security guard} that
prevents unwanted writes to disk.

You can preview files for development purposes in your server, and
save production-ready versions to disk using the distributor.
To do this, replace the first argument of @racket[existing-files]
with a new procedure:

@racketblock[
(require racket/hash)

(define assets/ (this-directory/ "assets"))

(define (on-file path)
  (hash-union (make-serveable path)
              (make-distributable path)))

(replace-resolver
  (existing-files on-file
                  (search-within assets/)))]

@racket[make-serveable] and @racket[make-distributable] don't exist
yet. We'll create them, but I want you to look at this code as
an abbreviation of the finished product.

The @racket[on-file] procedure decides what value represents a
(possibly changed) file located at some @racket[path]. Notice that it
glues together two hashes using @racket[hash-union], which should make
astute readers sweat a bit. Don't fret: UA leverages uninterned
symbols for keys, namely through @tech[#:doc '(lib
"hash-partition/scribblings/hash-partition.scrbl")]{hash partitions}.
It's harder to accidentally cause collisions with these keys, so don't
worry about data mixing in unacceptable ways.

Let's say we want to save our assets as copies of the input
file, and serve the output of @racket[get-file-info].

We'll start with the server. Use @racket[serveable] to tell the
development server how to serve some value. We already have bindings
for response structs at our disposal.

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

I use @racket[distributable] here to swap out the assets directory
with the dist directory in a given path. That way,
@tt{/path/to/assets/toffee.jpg} becomes @tt{/path/to/dist/toffee.jpg}.

@racketblock[
(require racket/path racket/port)

(define (make-distributable input-path)
  (distributable (this-directory/ "dist" (find-relative-path assets/ input-path))
                 (lambda (to-output-file)
                   (call-with-input-file input-path
                     (lambda (from-input-file)
                       (copy-port from-input-file to-output-file))))))]

With that, we have the full configuration.

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
  (hash-union
    (make-serveable path)
    (make-distributable path)))

(replace-resolver
  (existing-files on-file
                  (search-within assets/)))

(module+ main (u/a-cli))]

Let's see if it works. Fire up the server again. If you GET
@tt{/toffee.jpg}, you'll see information about my bunny's
picture. Not the picture itself. If you want to change that, then
change the @racket[serveable].

Now we'll use the distributor. Run @litchar{racket project.rkt
--distribute project.rkt}.  By default, the distributor performs a dry
run. It logs what it would have done without modifying the contents of
your disk. You should see an entry showing that Toffee will appear in
the dist directory. If not, then please
@hyperlink["https://github.com/zyrolasting/unlike-assets/issues"]{report
an issue}.  Otherwise, run the command again with the
@litchar{--no-dry-run} option to commit the changes to disk.

@verbatim|{
racket project.rkt --no-dry-run --distribute toffee.jpg
}|

This is the short form, if your hands are sore: @litchar{racket
project.rkt -nd toffee.jpg}

You will see that Toffee has been copied to the @tt{dist}
subdirectory.

In this section we expanded our configuration to support exporting
files, while serving a different representations of those files. We've
also adjusted the project to seek input in one directory, only to
spit out output in a different directory.


@section{What Do We Make of All This?}

There's nothing new or magical going on.

The only real difference is that you reason about resources in terms
of @italic{external assets} and @italic{how they are used}. This is a
particularly useful view that creative workspaces need to work well.

When you ask for an asset using @racket[procure], you control
what creates the value you want, and you can decorate any part
of the process with added features in data so it works in more
places.

That's it. It doesn't need to be any more than that.

Take five. You've earned a break. In the next section, we'll review
the same configuration with the hood up.

@section[#:tag "training-wheels-off"]{Take the Traning Wheels Off}

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

I added a @racket[resolve-file] procedure to pass to
@racket[replace-resolver]. Notice that the @tt{assets} directory is
now mentioned in a different place.

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

If you understand how the code works, then you will understand how to
read @other-doc['(lib "unlike-assets/scribblings/reference/unlike-assets-reference.scrbl")]

@section{What Now?}
From here, you are free to write your own resolver.

I'd hope that your configuration would look something like this:

@racketmod[#:file "project.rkt"
u/a

(require "my-stuff.rkt")

(replace-resolver
  (documents)
  (css)
  (spreadsheets)
  (shaders))

(module+ main (u/a-cli))]

I envision my own projects as being a composition of, well, unlike
assets.  If I want a web page with an embedded SPIR-V shader applied
to a sphere at build-time, then dammit, I want a web page with an
embedded SPIR-V shader applied to a sphere at build-time. Doing crazy
things like that requires something like UA.

On that note, Unlike Assets as a project contains more packages than
just @tt{unlike-assets}. The
@hyperlink["https://github.com/zyrolasting/unlike-assets"]{source
code} shows what I suppose you could call the "canonical" packages,
but how your resolver behaves is honestly up to you. I hope that
others will add their own extensions so that you can piece together
something especially cool with little effort.

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
