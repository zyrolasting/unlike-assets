#lang scribble/manual
@(require (for-label racket racket/class unlike-assets unlike-assets/policy))

@title{Policy authoring utilities}

@defmodule[unlike-assets/policy]

This module provides practical tools when working with @racket[unlike-compiler%].

@section{Clarification}

@defproc[(clarify/multi [compiler (instanceof/c (subclass?/c unlike-compiler%))] [unclear-names (listof unclear/c)]) (listof clear/c)]{
Maps unclear names to clear names using a compiler.
}

@defproc[(local-asset-url? [str string?]) boolean?]{
Returns @racket[#t] if @racket[str] is a URL that meets
ALL of the below criteria:

@itemlist[
@item{Has no scheme or a @racket["file"] scheme}
@item{Has no host, or a host that points to the loopback interface.}
@item{Has no port, credentials, query, or fragment}
@item{Has a path}
]

Use case: Distinguishing when URLs refer to a local filesystem.
}

@defproc[(file-url? [url-inst url?]) boolean?]{
Returns @racket[#t] if @racket[url-inst] has no scheme or a @racket["file"] scheme.
}

@defproc[(file-readable? [path path?]) boolean?]{
Returns @racket[#t] if @racket[path] refers to an existing file and read permissions are set for the process.
}

@defproc[(build-complete-simple-path [path (or/c string? path?)]
                                     [relative-to (or/c path? boolean?) #f])
                                     complete-path?]{
If @racket[relative-to] is a path, and @racket[path] is not complete, returns @racket[(simplify-path (build-path relative-to path))]. Otherwise returns @racket[(simplify-path path)].
}

@section{Asset Advancement and Regression}
@defproc[(chain [proc procedure?] [args any/c] ...) procedure?]{
Returns an @racket[advance/c] procedure that returns
the value of @racket[(apply proc args)].

Use to transmit dynamically bound values across @racket[advance/c] functions
or to define simpler procedures that do not have to comply with the domain
requirements of @racket[advance/c].

@racketblock[
(define (step2 extra) extra) @; fulfill with value 'data

(define/contract (step1 clear compiler) advance/c
  (chain step2 'data))
]
}

@defthing[block ripple/c]{
A ripple procedure that unconditionally prevents change from propogating.}
@defthing[rebuild ripple/c]{
A ripple procedure that unconditionally forces an asset to rebuild.}

