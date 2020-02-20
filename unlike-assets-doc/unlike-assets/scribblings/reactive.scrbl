#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    kinda-ferpy
                    unlike-assets/reactive
                    unlike-assets]]

@title{Reactive Model}

@defmodule[unlike-assets/reactive]
Unlike the imperative model, the reactive model uses no classes. It
also uses @racketmodname[kinda-ferpy] to represent assets as living
values.

@bold{This module is experimental. Use the imperative model
if you need a stable interface.}

@section{Reactive API Reference}
@defproc[(u/a-build-system? [p any/c]) boolean?]{
Returns @racket[#t] if @racket[p] is an unlike asset build system created by @racket[make-u/a-build-system].
}

@defproc[(live-build? [p any/c]) boolean?]{
Returns @racket[#t] if @racket[p] is a live build created by @racket[start-live-build!].
}

@defproc[(start-live-build! [key string?]
                            [#:sample! sample! (-> any/c)]
                            [#:build! build!  (-> any/c any/c)]
                            [#:suppress? suppress? (-> any/c any/c any/c?) eq?])
  live-build?]{
Returns a procedure @racket[LB] that encapsulates a value
that changes due to external factors.

When you apply @racket[start-live-build!], you also apply
@racket[sample!], then @racket[build!]. @racket[build!]
will run on its own thread.

Specifically, this procedure creates two stateful cells:

@itemlist[
@item{@racket[%S], which holds the last known value of @racket[(sample!)].}
@item{@racket[%B], which is an async cell created using @racket[(make-stateful-cell/async #:dependencies (list %S)
(lambda () (build! (%S))))].}
]

Each time you apply @racket[LB], it will apply @racket[sample!] and compare
the returned value to @racket[%S] using @racket[suppress?]. If
@racket[suppress?] returns a false value, then @racket[%S] will
update to the new value and cause @racket[%B] to update.

@racket[(LB)] will return @racket[%B]. You can depend on this cell to
monitor when the build thread restarts.

@margin-note{Why have @racket[stop?] at all? Because @racket[(%B)]
returns a procedure that waits for the build thread to finish. This
means @racket[((%B))] waits for the build output. If that build output
is a procedure--which is normal when resolving dependencies--then
getting to the final value means writing a comical expression with at
least three bracket pairs: @racket[(((%B)))]. @racket[stop?]
allows you to search for a common representation of build output.}
@racket[(LB stop?)] will apply @racket[(stop? %B)], then
@racket[(stop? (%B))], @racket[(stop? ((%B)))], and so on until
it encounters a true result. The value @racket[V] that made @racket[(stop? V)]
true value is the value returned. This implies that @racket[(LB)] is equivalent
to @racket[(LB stateful-cell?)].
}

@defproc[(make-u/a-build-system [key->live-build (-> any/c u/a-build-system? live-build?)]) u/a-build-system?]{
Returns a procedure @racket[S] that encapsulates a mutable hash of live builds.

@racket[(S key)] will return a live build with the given key.
If the build does not already exist in the hash, then it will first be
added using @racket[(key->live-build key S)]. Subsequent applications of
@racket[S] to @racket[key] will return the same reference to the live
build without consulting @racket[key->live-build].

@racket[(S key stop?)] behaves like @racket[(S key)], but will also
apply the live build procedure @racket[LB] to @racket[stop?] (See
@racket[start-live-build!]).

@racket[(S)] will return the hash of live builds encountered over its
lifetime.

Notice that @racket[S] can be invoked recursively in the body of
@racket[key->live-build]. This allows one live build to depend on
other live builds. Be warned that if a cycle forms (e.g. the build for
key @racket["a"] in turn applies @racket[S] to @racket["a"]), then the
build will not terminate.
}

@section{Example: Living Values based on Files}

@racketblock[
(require racket/runtime-path)

(code:comment "Base change detection on file modification seconds.")
(define (start-live-file-build! key build!)
  (start-live-build! key
    #:sample!   (λ () (file-or-directory-modify-seconds key))
    #:suppress? =
    #:build!    build!))

(define (start-live-line-count key u/a)
  (start-live-file-build! key
                          (λ (mtime) (length (file->lines key)))))

(define (start-live-mtime key u/a)
  (start-live-file-build! key identity))

(define (key->live-build key u/a)
  (define start!
    (if (string-suffix? key ".txt")
        start-live-line-count
        start-live-mtime))
  (start! key u/a))

(define sys (make-u/a-build-system key->live-build))

(code:comment "This will return the current modification time of dummy.bin.")
(sys "dummy.bin" number?)
(code:comment "This will return the current number of lines in passage.txt.")
(sys "passage.txt" number?)
]
