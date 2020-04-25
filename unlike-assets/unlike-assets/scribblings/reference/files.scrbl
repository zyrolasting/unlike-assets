#lang scribble/manual

@title{@tt{unlike-assets/files}}

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    unlike-assets/resolver
                    unlike-assets/files]]

@defmodule[unlike-assets/files]

@racketmodname[unlike-assets/files] extends
@racketmodname[unlike-assets/resolver] to treat files as assets.

@section{Asset Definitions}
@defthing[asset/file-sourced/c (and/c (asset/c asset/with-read/c [input-file-path complete-path?]))]{
Confirms if an asset can be read from a file system.
}

@defthing[asset/file-destined/c (and/c asset/with-write/c (asset/c [output-file-path complete-path?]))]{
Confirms if an asset can be written to a file system.
}

@defthing[asset/file-to-file/c (and/c asset/file-sourced/c asset/file-destined/c)]{
You get the idea.
}

@section{Resolving Filesystem Paths}
@defmodule[unlike-assets/files/resolve]

@defproc[(find-file-path [#:must-exist must-exist boolean? #t]
                         [p (or/c path-for-some-system? path-string?)]
                         [search-dirs (non-empty-listof complete-path?) (list (current-directory))])
                         (or/c #f complete-path?)]{
If @racket[p] is complete, this returns @racket[p].  Otherwise, this
returns the first path built from each directory in
@racket[search-dirs] to refer to an existing file.

If no file is found, the result depends on @racket[must-exist].  If
@racket[must-exist] is a true value, this will raise
@racket[exn:fail:filesystem]. Otherwise, the result is @racket[#f].
}

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

@defproc[(procure/weak/relative-path-string [key string?]) string?]{
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


@section{Integrating with the Asset Resolver}
@defproc[(file->asset [p complete-path?]) asset/file-sourced/c]{
Returns an asset that represents a file.
}

@defproc[(make-key->live-build [search-dirs (non-empty-listof complete-path?) (list (current-directory))])
         procedure?]{
Returns a procedure @racket[P] suitable for use in @racket[current-key->live-build].

@racket[(P key sys)] returns @racket[#f] if the key is not applicable
as a path or path string for some filesystem.

Otherwise, it returns a living build that maintains the latest value
of @racket[(file->asset (find-file-path search-dirs))].

Change detection is based on
@racket[file-or-directory-modify-seconds], which means that the asset
can be up to one second out of date.
}
