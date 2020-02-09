#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    kinda-ferpy
                    unlike-assets/reactive
                    unlike-assets]]

@title{Reactive Model}

@defmodule[unlike-assets/reactive]
Unlike the imperative model, the reactive model uses no classes.  It
also uses @racketmodname[kinda-ferpy] to represent assets as living
values. This is a better fit for larger scale projects or for those
who prefer writing build procedures as pure functions that can defer
certain side-effects.

@section{Illustrative Example}

@racketblock[
(require racket/runtime-path)

(define-runtime-path assets-path "./assets")

(define (key->live-build key)
  (define file-path (simplify-path (build-path assets-path key)))
  (start-live-build (λ ()
                      (with-handlers ([exn:fail:filesystem? identity])
                        (file-or-directory-modify-seconds file-path)))
                    (λ (prev next)
                      (file->string file-path))))

(define sys (make-u/a-build-system key->live-build))

(define %living-home-page (sys "index.md"))
(define %living-about-page (sys "about.md"))
(define %living-stylesheet (sys "styles.css"))
]

Here, all of the identifiers starting with @racket[living-] are bound
to living builds that represent the latest version of their file once
observed. Each living build can depend on other living builds.

@section{Reactive API Reference}
@defproc[(u/a-build-system? [p any/c]) boolean?]{
Returns @racket[#t] if @racket[p] is an unlike asset build system created by @racket[make-u/a-build-system].
}

@defproc[(live-build? [p any/c]) boolean?]{
Returns @racket[#t] if @racket[p] is a live build created by @racket[start-live-build].
}

@defproc[(start-live-build [sample-change (-> any/c)]
                           [respond (-> any/c any/c)]
                           [suppress? (-> any/c any/c boolean?) eq?])
  live-build?]{
Returns a procedure @racket[LB] that encapsulates a living build.

It creates two @racketmodname[kinda-ferpy] cells: One cell is
asynchronous, meaning it's value is equal to a Racket @racket[thread]
working to compute @racket[(respond)]. @racket[(LB)] returns the
reference to this async cell.

The other cell is synchronous and acts as a signal for the
asynchronous cell to start anew, since the async cell depends on the
signal cell.  The synchronous cell initializes with the value of
@racket[(sample-change)], and any repeated applications of @racket[LB]
will update the signal cell with @racket[(sample-change)] unless
@racket[suppress?] returns a true value.

Nearly equivalent to the below:

@racketblock[
(define (start-live-build sample-change respond [suppress? eq?])
  (define initial-value (sample-change))
  (define %signal (stateful-cell initial-value))
  (define %producer
    (make-stateful-cell/async
     #:dependencies (list %signal)
     (λ () (respond (%signal)))))
   (λ ()
     (define next-value (sample-change))
     (unless (suppress? (%signal) next-value)
       (%signal next-value))
     %producer))
]
}

@defproc[(make-u/a-build-system [key->live-build (-> any/c live-build?)]) u/a-build-system?]{
Returns a procedure @racket[S] that encapsulates a mutable hash of live builds.

@racket[(S key)] will return a live build with the given key. If the
build does not already exist in the hash, then it will first be added
using @racket[(key->live-build key)]. Subsequent applications of
@racket[S] to @racket[key] will return the same reference to the live
build without consulting @racket[key->live-build].

@racket[(S)] will return the hash of live builds.
}

@deftogether[(
@defproc[(get-live-build [sys u/a-build-system?] [u any/c]) live-build?]
@defproc[(maybe-build-u/a-cell! [sys u/a-build-system?] [u any/c]) stateful-cell?]
@defproc[(wait-for-u/a-cell! [sys u/a-build-system?] [u any/c]) (-> any/c)]
@defproc[(procure-u/a! [sys u/a-build-system?] [u any/c]) any/c])]{

Given a build system @racket[S], @racket[(S key)] represents a living
asset by name.

@itemlist[
@item{@racket[get-live-build], or @racket[(S "index.md")], returns the live build monitoring @tt{index.md}.}
@item{@racket[maybe-build-u/a-cell!], or @racket[((S "index.md"))], returns the async cell representing the build results for @tt{index.md}}
@item{@racket[wait-for-u/a-cell!], or @racket[(((S "index.md")))], returns a procedure that waits on and then returns the build results of the async cell.}
@item{@racket[procure-u/a!], or @racket[((((S "index.md"))))], returns the current representation of @tt{index.md}}
]
}
