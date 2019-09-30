#lang scribble/manual
@(require (for-label racket racket/class unlike-assets unlike-assets/logging unlike-assets/policy))

@title[#:tag "live"]{Responding to Change}

With some work, an @racket[unlike-compiler%] instance can support ongoing builds that perform only the work
thats relevant to reported changes.

@section{Overview}

Once an @racket[unlike-compiler%] instance finishes a call to @method[unlike-compiler% compile!], it will
remember the history of each asset value as it advanced to a fulfilled state.

If you call @method[unlike-compiler% compile!] again, it will simply return the same output it already prepared
unless you report that there were changes.

@racketblock[
(send compiler compile!
  #:changed (clarify/multi compiler "/etc/config")
  #:removed (clarify/multi compiler
                           "leaked-xmas-photo.png"
                           "only-production-database-backup.sql"))
]

Unlike @method[unlike-compiler% add!], which can be called at any time, @racket[#:changed] and @racket[#:removed]
assets can only be declared after a prior successful call to @method[unlike-compiler% compile!]. Each following
call to @method[unlike-compiler% compile!] will adapt an underlying graph to reflect any assets that were
since changed or removed. Depending on the activity, dependent assets will @italic{regress} to a value they once
had in an attempt to incorporate changes while minimizing rework.

First, @racket[#:removed] assets are deleted from the graph, as well as any edges connecting that
that asset to others. Dependent assets are unconditionally rebuilt, but changes ripple from there
according to @secref["ripproc"].  The compiler will raise @racket[exn:fail] if any of the names
marked as removed are already absent from the graph.

@racket[#:changed] assets then regress to a value returned from @method[unlike-compiler% delegate].
This is functionally equivalent to marking an asset to rebuild from scratch. Changes will ripple to
dependents according to @secref["ripproc"]. This process will raise @racket[exn:fail] if any
@racket[#:changed] assets that were @bold{not} marked as removed are absent from the graph.


@section[#:tag "ripproc"]{Ripple Procedures}

Dependent asset values regress according to a related @racket[ripple/c] procedure
provided to @method[unlike-compiler% add!].


@defthing[ripple/c (-> clear/c clear/c (non-empty-listof unlike-asset/c) unlike-asset/c)]{

Returns a regressed asset value due to a change in a dependency.

The first argument is the clear name of the asset that has been changed.

The second argument is the clear name of the dependent asset.

The third argument is the history of the the dependent such that the element at index N
is an @racket[unlike-asset/c] returned by the @racket[advance/c] element at index N+1.

@bold{The ripple procedure must return an element from this list.} Otherwise the compiler
will ignore the value, issue a @racket[<warning], and rebuild the dependent asset from scratch.

Internally, the compiler will only propogate a change further if the regressed value
is not @racket[eq?] to its previous value. The first element of the asset value's history
is always @racket[eq?] to its current value, so having a @racket[ripple/c] procedure
return the first element of the history will both maintain the current value of the asset
and prevent change from propogating further.

@section{Step-by-Step Example}

@racketblock[
(define (replace-link-node) '...)
(define (immune changed/clear dependent/clear dependent/history) (car dependent-history))
(define (always-rebuild changed/clear dependent/clear dependent/history) (last dependent-history))
(define (partial changed/clear dependent/clear dependent/history) replace-link-node)

(send compiler add! "index.html")
(send compiler add! "styles.css")
(send compiler add! "about.html" "index.html" always-rebuild)
(send compiler add! "contact.html" "about.html" immune)

(send compiler add! "styles.css" "about.html" partial)
(send compiler add! "styles.css" "contact.html" partial)

(send compiler compile!)
]

In this example we compile a website of interdependent pages and
walk through the behavior of each subsequent build.

@racketblock[
(send compiler compile! #:changed '("about.html"))
]

Compile assuming that @racket["about.html"] has changed.

Rebuilds both @racket["about.html"] and @racket["index.html"].

@racketblock[
(send compiler compile! #:changed '("contact.html"))
]

The website is rebuilt assuming only @racket["contact.html"] has changed.
The relationship between @racket["contact.html"] and @racket["about.html"]
is such that the latter @bold{will not change} when the former changes. Not only
that, @racket["index.html"] will also @bold{not change} because propagation
stopped at @racket["about.html"]. In this scenario, only one webpage is fully rebuilt.

@racketblock[
(send compiler compile! #:changed '("styles.css"))
]
The website is rebuilt assuming only @racket["styles.css"] has changed.
In this scenario, @racket["about.html"] and @racket["contact.html"] are @italic{partially} rebuilt assuming that
@racket[replace-link-node] is in their respective histories. But @racket["index.html"] is still
fully rebuilt becase @racket["about.html"] propagates change using @racket[always-rebuild].

@racketblock[
(send compiler compile! #:removed '("about.html"))
]

The website is rebuilt assuming only @racket["about.html"] was removed.
@racket["about.html"] sat between @racket["index.html"] and @racket["contact.html"],
which means that @racket["contact.html"] is no longer a dependency for any HTML asset.
The compiler will still attempt to advance all assets, but does not guarentee that the
output will function as expected. This state will likely result in a broken or missing link
on a web page.

@racket["index.html"] is marked to build from scratch, but not because of its declared
relationship with @racket["about.html"] this time around. Asset removal causes a full
rebuild of dependents. If @racket["index.html"] had dependents, the change implied
by this rebuild would ripple normally.

@section{Design Implications}

@itemlist[
@item{Always rebuilding dependents of a removed asset allows @racket[ripple/c] procedures
to assume that dependencies exist. Any errors that come as a result of removal would
happen as per the user's implementation and change propogation should not make it harder to find the root cause.}
@item{@bold{CAUTION:} If you make a new instance of an @racket[unlike-compiler%] subclass and start
a fresh build using the same assets, it might not produce the same graph as an existing instance
that removed assets from an older graph. This is a source of entropy that may warrant periodically
discarding a compiler instance, collecting garbage, and beginning anew.}]
}
