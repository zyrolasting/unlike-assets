#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/runtime-path
                    unlike-assets]]

@title{Configuration}

@defmodule[unlike-assets/config]

@racketmodname[unlike-assets/config] provides all bindings from
@racketmodname[racket/runtime-path].

@defform[(this-directory/ path-el ...)]{
Like @racket[build-path], except the first element of the path
is the directory of the file in which this expression appears.

If used in a location where a directory cannot be inferred (such as a REPL),
then the path is built in terms of @racket[(current-directory)].
}

@defform*[((nearest-u/a) (nearest-u/a file-name))]{
When used in @racket[require], this will search for the nearest
@litchar{#lang u/a} file. The directory in which this expression
appears is searched, followed by all readable parent directories.

The first form, lacking a file name for a hint, will check all
readable files in each directory. The second form will only look for
files with the exact file name. (e.g. @racket[(require (nearest-u/a
"config.rkt"))].
}

@defproc[(use [#:cache cache seat-cache/c ((current-seat))] [resolvers resolver/c] ...) void?]{

Replaces @racket[current-seat] with a new @tech{seat} bound to the
given @racket[resolvers].

Equivalent to:

@racketblock[(current-seat (make-seat (apply rlist resolvers) cache))]

}