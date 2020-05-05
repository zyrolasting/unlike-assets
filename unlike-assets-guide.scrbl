#lang scribble/manual

@require[@for-label[racket/base]]

@title{Unlike Assets: Building a Polyglot Alternative}
@author{Sage Gerard}

This guide follows [[Unlike Assets Guide]]. Here, we will implement an
alternative to @racketmodname[polyglot] from scratch. In a sense, this
is yet another blog tutorial. But unlike (heh) other blog tutorials,
we'll be done in five minutes.

If you would rather not read this guide, you can view the source of
the finished result on GitHub.  Otherwise, stick with me and we'll
write that code together.

@section{Setup}
We'll need a few packages to get started.

@verbatim|{
$ raco pkg install unlike-assets unlike-markdown unlike-css unlike-cache-busters
}|

@tt{unlike-markdown} and @tt{unlike-css} contain modules
that make their respective formats useful to a custom resolver.
Next, make a resolver that understands Markdown, CSS, and
cache-busting files.

@racketmod[#:file "project.rkt"
u/a

(define search (search-within (this-directory/ "assets")))
(define (dist-rule f)
  (distribute-to (this-directory/ "dist") f))

(require "markdown.rkt"
         "css.rkt"
         "cache-busters.rkt")

(replace-resolver
  (markdown)
  (css)
  (cache-busters))

(module+ main (u/a-cli))]

Now we'll write each @racket[require]d module.

@section{Markdown}


@racketmod[#:file "markdown.rkt"
u/a

(require unlike-markdown)

(define (markdown)
  (markdown-modules
    ))

(module+ main (u/a-cli))]



In this configuration, the resolver will seek out Markdown documents,
CSS modules, and static files in an @tt{assets} directory. User-facing
variants will be distributed to the @tt{dist} subdirectory, just like
in Polyglot.

@section{Writing a Page}

A Polyglot project can therefore contain Markdown

To do this, let's create a Markdown file that can define
its own layout and depend on images (as opposed to just
referencing them).

Let's try a new configuration that demonstrates what I mean.

Make a Markdown document that says this:

@verbatim|{

<script type="text/racket+layout">
'(html (head "Title")
       (title))
</script>

}|

This resolver will dynamically load Racket modules with reload
support, in keeping with @racket[procure]'s intended purpose.
The default configuration will simply collect all run-time
(Phase 0) exports of a module and present them as a hash-table.



@section{Start a New Project}
Write this code into a file in a fresh directory. You can call
the file whatever you'd like, but I'll call it @tt{project.rkt}
here.

@racketmod[u/a

(module+ main (u/a-cli))]

This module doubles as a configuration for @tt{unlike-assets}, and as
an entry point for a project under that configuration.  We're using a
default command-line interface.  Don't worry if you end up not liking
it, since you can always substitute your own later.

The default CLI comes with a server and a file sync tool that you can
use export your project to disk. You can inspect your options with
@litchar{racket project.rkt -h} as always.

You can also run this module in your REPL and interact with your
assets directly, but more on that in a bit. Right now we have an
empty configuration, and that won't do.

@section{Add Asset Definitions}

Let's bring in the add-on packages we installed earlier.

@racketmod[u/a

(require unlike-assets/markdown
         unlike-assets/css)

(u/a (markdown-modules)
     (css-modules))

(module+ main (u/a-cli))]

The @racket[u/a] procedure is a fun one. It installs a new module
resolver that understands the data formats of the add-ons you
installed. If it's set up right, you can do some pretty neat things
with it.

Each add-on starts life stupid. We need to tell the CSS and Markdown
add-ons to look in a specific directory.  I'll tell them both to look
in an @tt{assets} directory using @racket[define-runtime-path], and
@racket[within-directories].

@racketmod[u/a

(define-runtime-path assets/ "assets")
(define find-path (within-directories assets/))

(require unlike-assets/markdown
         unlike-assets/css)

(u/a (markdown-modules find-path)
     (css-modules find-path))

(module+ main (u/a-cli))]

This makes a resolver that looks in a nearby @tt{assets/} directory
for Markdown and CSS files.

@section{Procuring Our First Files}

Make the @tt{assets} directory and plop down a Markdown file and
a CSS file.

Here's some Markdown you can copy. Yes it looks weird, but you'll
like where this is going.

@verbatim|{
<meta title="" />

# Blabbity Blee Blah

Blabbity _blahhhh_
}|

And here's some CSS.

@verbatim|{
h1 { color: blue }
}|

I'll paste them into @tt{page.md} and @tt{styles.css}, and save those
two files into the @tt{assets} directory.

Then, Using DrRacket or your @tt{racket} launcher, start up a REPL
inside of @tt{project.rkt}.

@racketinput[(procure "styles.css")]
@racketresult[#<procedure:asset>]
@racketinput[(procure "page.md")]
@racketresult[#<procedure:asset>]

Lookie there, it found them. I'm oversimplifying to the point of being
rude, but think of @racket[procure] as your version of
@racket[require]. It uses your configuration to present your creative
assets as if they were dynamic modules. To see what we have to work
with, apply one of the asset proceduress.

@racketinput[
(define my-page (procure "page.md"))
(my-page)]
@racketresult[
#hash((output-file-path . "page.html")
      (make-http-response . #<procedure>)
      (write-file . #<procedure>)
      (render-doc . #<procedure>))
]

Applying any asset procedure will make it simply dump what it has for
your review. What an asset has depends on the asset type, but
you'll come to expect a few standard bits.

Look at me, I'm blathering. I should be showing you the cool parts.

@section{}

@section{Using the Project Server}
@tt{racket project.rkt -sv} will kick up a project server. By default,
every request you send it will be processed by @racket[procure].

But if we send this server any request at all, it will raise an error.

<show error>

This error is complaining that we did not implement our module
resolver. Which is fair, because we didn't.
