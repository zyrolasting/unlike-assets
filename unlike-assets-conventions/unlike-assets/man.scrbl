#lang scribble/manual

@title{Unlike Asset Conventions}

@require[@for-label[racket/base]]
@defmodule[unlike-assets/conventions]

This module defines conventions for the optional @tt{unlike-assets-*}
packages.

@section{Contracts}
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

@defthing[asset/http-responder/c  (asset/c (->http-response (-> request? response?)))]{
Checks for assets that include a handler for mapping an HTTP request to a response.
}

@defthing[asset/webdev/local/c  (and/c asset/file-to-file/c
                                       asset/writable/c
                                       asset/http-responder/c)]{
Checks for assets that express the characteristics normally expected
in a local web development project.
}


@section{Procedures}

@defproc[(procure/strong/relative-path-string [production-path complete-path?] [key string?]) string?]{
Returns a relative path string that a file located at
@racket[production-path] can use to refer to the file that the asset
of @racket[key] would produce.

If the asset procured with @racket[key] does not match @racket[asset/file-destined/c], then
this procedure will raise @racket[exn:fail:contract?].

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

@defform[(define-relative-dependency-lookups input-path-expr
          #:contracts [input-path-expr complete-path?])]{
Expands to the following conventional abbreviations of
@racket[procure/weak/keep-key] and
@racket[procure/strong/relative-path-string].

@racketblock[
(define Ps& (curry procure/strong/relative-path-string input-path-expr))
(define Pw& procure/weak/keep-key)
]

An incomplete asset can use this macro to orchestrate
dependencies with a procure function.

@racketmod[scribble/custom

Assume @tt{define-relative-dependency-lookups} was
included when reading this module.

I started building @Pw&{styles.css}, just by mentioning it here.

The production-facing path is @Ps&{styles.css}.
]
}
