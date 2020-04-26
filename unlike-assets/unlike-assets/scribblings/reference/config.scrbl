#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    unlike-assets]]

@title{@tt{unlike-assets/config}}
@defmodule[unlike-assets/config]

@defform[(this-directory/ path-el ...)]{
Like @racket[build-path], except the first element of the path
is the directory of the file in which this expression appears.

If used in a location where a directory cannot be inferred (such as a REPL),
then the path is build in terms of @racket[(current-directory)].
}


@defform*[((nearest-u/a) (nearest-u/a file-name))]{

When used in @racket[require], this will search for the nearest
@litchar{#lang u/a} file. First the directory in which this expression
appears is searched, followed by all readable parent directories.

The first form, lacking a file name for a hint, will check all
readable files in each directory. The second form will only look for
files with the exact file name. (e.g. @racket[(require (nearest-u/a
"config.rkt"))].
}


@defproc[(replace-resolver [key->maybe-thunk (-> any/c (or/c #f (-> any/c)))] ...) void?]{
Imperatively replaces @racket[current-resolver] with a new resolver,
while preserving the cache.

This procedure combines several procedures into a single
@racket[key->thunk] argument for @racket[make-resolver].  If one
@racket[key->maybe-thunk] returns @racket[#f], the next one is
consulted for a procedure. If no procedure is found for all values of
@racket[key->maybe-thunk], then the combined procedure will raise an
@racket[exn:fail].
}
