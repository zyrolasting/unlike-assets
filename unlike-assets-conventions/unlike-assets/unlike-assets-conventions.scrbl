#lang scribble/manual

@title{@tt{Conventions for Unlike Assets}}

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    racket/rerequire
                    kinda-ferpy
                    unlike-assets/core
                    unlike-assets/conventions]]
@defmodule[unlike-assets/conventions]

This module imposes useful conventions shared by all optional
@tt{unlike-assets-*} packages.

@racketmodname[unlike-assets/conventions] reprovides
@racketmodname[unlike-assets/core],
@racketmodname[unlike-assets/conventions/resolver],
@racketmodname[unlike-assets/conventions/procedures], and
@racketmodname[unlike-assets/conventions/contracts].

@section{Asset Resolver}
@defmodule[unlike-assets/conventions/resolver]

@racketmodname[unlike-assets/conventions/resolver] is meant to behave
as Racket would if it could evaluate something like this:

@racketblock[(begin (dynamic-rerequire "index.js")
                    (dynamic-require "index.js" 'minified))]

Even so, this is a poor-man's module resolver. It does not produce
Racket modules, but it does let you write expressions like @racket[(Ps
"index.js" 'minified)] to import non-Racket data.

@defthing[current-u/a-build-system (parameter/c u/a-build-system?)]{
This is a globally shared build system. You need one of these to give
the impression of an omnipresent module resolver. The default value of
this parameter is a build system that uses
@racket[(current-key->live-build)] to resolve builds.

Since build systems accrue assets over time, you can reclaim some
memory as follows, assuming you do not have references to assets lying
around:

@racketblock[
(code:comment "Replace the shared build system with a new one")
(current-u/a-build-system
 (make-u/a-build-system
  (lambda (key recurse)
   ((current-key->live-build) key recurse))))
(collect-garbage ...)
]
}

@defthing[current-key->live-build  (parameter/c (-> string? u/a-build-system? live-build?))]{
A shared procedure that controls how keys map to living builds.

The default value for @racket[current-key->live-build] raises an
error that instructs you to provide your own handler.
}

@defproc[(procure/weak [key string?]) stateful-cell?]{
Equivalent to @racket[((current-u/a-build-system) key stateful-cell?)].

This starts an asynchronous build for an asset (if needed), but does
not wait for the result. Returns the async cell representing the thread
that builds the asset.
}

@defproc[(procure/strong [key string?] [sym symbol?] ...) any/c]{
Equivalent to:

@racketblock[(apply (make-u/a-procure-procedure (current-u/a-build-system)) key syms)]

This starts a build for an asset (if needed), waits for the results,
then returns requested data.
}

@defproc[(procure/strong/with-contract [key string?] [c contract?] ...) asset?]{
Like @racket[procure/strong], but this does not extract individual values
from an asset. It instead waits for and returns the asset by @racket[key],
and raises @racket[exn:fail:contract] if that asset does not fulfil @racket[c].
}

@deftogether[(
@defthing[Pw procure/weak]
@defthing[Ps procure/strong]
@defthing[Ps/c procure/strong/with-contract]
)]{
You'll probably use the @tt{procure/*} procedures often enough to want
these abbreviations.
}

@defproc[(make-key->live-build/sequence [maybe-makers (-> string?
                                                          u/a-build-system?
                                                          (or/c #f live-build?))] ...)
                                        procedure?]{
Returns a procedure equivalent to the following:

@racketblock[
(λ (key recurse)
   (ormap (λ (p) (p key recurse))
          maybe-makers))]

You can use this to sequence several procedures that map keys to live builds.
}

@section{Contracts}
@defmodule[unlike-assets/conventions/contracts]

@deftogether[(
@defthing[asset/writable/c (asset/c [write-bytes (-> output-port? any)])]
@defthing[asset/writeable/c asset/writable/c]
)]{
Checks for assets that provide a procedure to dump a representation to an output port.
}

@deftogether[(
@defthing[asset/file-sourced/c (asset/c [input-file-path (and/c complete-path? file-exists?)])]
@defthing[asset/file-destined/c (asset/c [output-file-path complete-path?])]
@defthing[asset/file-to-file/c  (and/c asset/file-sourced/c asset/file-destined/c)]
)]{
Checks for assets that depend on the file system.
}

@deftogether[(
@defthing[asset/servable/c  (asset/c (->http-response (-> request? response?)))]
@defthing[asset/serveable/c asset/servable/c]
)]{
Checks for assets that include a handler for mapping an HTTP request to a response.
}

@defthing[asset/webdev/local/c  (and/c asset/file-to-file/c
                                       asset/writable/c
                                       asset/serveable/c)]{
Checks for assets that express the characteristics normally expected
in a local web development project.
}


@section{Procedures}
@defmodule[unlike-assets/conventions/procedures]

@defproc[(procure/strong/relative-path-string [production-path complete-path?] [key string?]) string?]{
Returns a relative path string that a file located at
@racket[production-path] can use to refer to the file that the asset
of @racket[key] would produce.

If the asset procured with @racket[key] does not match @racket[asset/file-destined/c], then
this procedure will raise @racket[exn:fail:contract].

When curried with a complete path, you can compute production-ready paths while building
an asset.

@racketblock[
(define & (curry procure/strong/relative-path-string "/srv/www/index.html"))
(& "styles.css") (code:comment "Something like \"styles/81a24d.css\"")
]
}

@defproc[(procure/weak/keep-key [key string?]) string?]{
Equivalent to @racket[(begin (procure/weak key) key)].

Useful for triggering a background build for an asset when you know that
@racket[key] also functions as a production-ready path.

@racketblock[
(code:comment "Same as href=\"about.html\", but the about page will start building as a side-effect.")
`(a ((href ,(procure/weak/keep-key "about.html"))) "About")
]
}

@defform[(define-relative-dependency-lookups input-path-expr)
          #:contracts ([input-path-expr complete-path?])]{
Expands to the following conventional abbreviations of
@racket[procure/weak/keep-key] and
@racket[procure/strong/relative-path-string].

@racketblock[
(define Ps& (curry procure/strong/relative-path-string input-path-expr))
(define Pw& procure/weak/keep-key)
]

An incomplete asset can use this macro to orchestrate
dependencies with a procure function.
}
