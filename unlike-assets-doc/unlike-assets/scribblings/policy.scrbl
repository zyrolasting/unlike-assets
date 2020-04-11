#lang scribble/manual
@(require (for-label racket
                     racket/class
                     unlike-assets
                     unlike-assets/policy
                     unlike-assets/reactive))

@title{Policy authoring utilities}

@defmodule[unlike-assets/policy]

This module provides helper procedures for the available models.

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

@section{Reactive Model Helpers}

@defproc[(make-key->live-build/sequence [maybe-makers (-> string? procedure? (or/c #f live-build?))] ...) procedure?]{
Returns a procedure equivalent to the following:

@racketblock[
(λ (key recurse)
   (ormap (λ (p) (p key recurse))
          maybe-makers))]

Use this to sequence several procedures that map keys to live builds.
}

@defproc[(make-key->live-build/unlike-compiler [instance (instanceof/c (subclass?/c unlike-compiler%))]
                                               [available? (-> clear/c boolean?)]
                                               [changed? (-> clear/c boolean?)])
                                               procedure?]{
Returns a procedure @tt{P} suitable for use in @racket[make-u/a-build-system],
where @racket[P] acts as an adapter between the reactive model and the imperative
model. @racket[(P key recurse)] assumes that @racket[key] is an unclear name. Once
clarified using @racket[(send instance clarify key)], the clear name is
checked against @racket[available?] and @racket[changed?]. From there,
@racket[P] will call @method[unlike-compiler% compile!] on the provided
instance as a side-effect with appropriate arguments.

The live build produced by any application of @racket[P] will always
produce the latest result of @method[unlike-compiler% compile!].
}
