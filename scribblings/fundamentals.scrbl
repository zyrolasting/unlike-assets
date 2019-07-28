#lang scribble/manual

@title{Fundamentals}

@defstruct[dependent ([val any/c] [dependencies (listof string?)])]{
Represents data or a procedure that cannot run until dependencies
are available for its use.

A @racket[dependent] is either @italic{fulfilled} or @italic{unfulfilled}. The
implementation details must follow only these rules:

@itemlist[
@item{If fulfilled, @racket[val] is not a procedure. This state is final.}
@item{If unfulfilled, @racket[val] is a procedure that returns a new, @italic{advanced} @racket[dependent] in an unfulfilled or fulfilled state.}
@item{All dependencies must be fulfilled before advancing a @racket[dependent] (This disallows circular dependencies).}]

@racket[dependencies] are @italic{unclear} strings that refer to other
instances of @racket[dependent]. Because this collection deals in assets with
arbitrary relationships, unclear strings must be made unique and unambiguous
(or @italic{clear}) using a user-provided @racket[clarify] function with @racket[compile-unlike].
More on that in a moment.}

@deftogether[(
@defthing[clear/c (generic-instance/c gen:equal+hash)]
@defstruct[(unlike-asset dependent) ([requested-as clear/c])]
)]{
An @racket[unlike-asset] is a @racket[dependent] with a clear name.
The exact data type used for clear names only matters to the extent that it
can be compared with @racket[equal?] and be usable in a @racket[dict?]. Most
of the time this can just be strings, but paths or other constrained types
may also be useful.}

@defproc[(compile-unlike [clarify (-> string? clear/c)]
                         [resolve (-> clear/c unlike-asset)]
                         [entry string?])
                         dict?]{
@margin-note{@racket[compile-unlike] returns @bold{all} encountered assets. You may need to filter out unwanted entries to create custom distributions.}
Advances the @racket[unlike-asset] produced by @racket[(resolve (clarify entry))] until it is fulfilled
on the current thread. If any advanced asset lists new dependencies, those dependencies will be processed before returning to advance the dependent asset.

Depending on your requirements and the complexity of your project, you may need to use custodians,
threads, engines, places, or other constructs to coordinate different calls to @racket[compile-unlike].

Returns a @racket[dict?] with @racket[clear/c] keys with all fulfilled assets encountered in the process as values.

The @racket[clarify] function accepts an @italic{unclear} string and returns @racket[clear/c]. If your
assets all sit on a mounted file system, your representation will likely be complete, simplified paths. 

@racketblock[
(define-runtime-path project-directory ".")

(define (unclear->clear unclear)
  (define path (build-complete-simple-path unclear project-directory))
  (unless (file-readable? tries)
    (error 'clarify "Cannot clarify ~a~n  Path not readable: ~a" unclear path)))
]

The @racket[resolve] function maps an output value from @racket[clarify] to
a new @racket[unlike-asset] instance. You can use this oppurtunity to make decisions
about what procedure is responsible for producing the first advanced version
of the asset. Here is a complete example using some hypothetical procedures.

@racketblock[
(define-runtime-path project-directory "./assets")

(define (unclear->clear unclear)
  (define path (build-complete-simple-path unclear project-directory))
  (unless (file-readable? tries)
    (error 'clarify "Cannot clarify ~a~n  Path not readable: ~a" unclear path)))

(define (clear->asset clear)
  (unlike-asset
    (case (path-get-extension path)
        [(#".md") markdown->dependent-xexpr]
        [(#".css") css->dependent-cssexpr]
        [else copy-file/sha1-name])
    null
    clear))

(compile-unlike unclear->clear clear->asset "index.md")
]

Note for emphasis that @racket[entry] is considered an @italic{unclear} dependency,
and is subject to resolution against @racket[clarify]. In the above example, @racket["index.md"]
is resolved relative to the @racket["./assets"] runtime path. This is done for behavioral
consistency, but it is easy to forget this detail when using the command line:

@verbatim[#:indent 2]|{
$ raco build-unlike my-policy.rkt ./assets/index.md # whoops
Cannot clarify ./assets/index.md
  Path not readable: /home/sage/project/assets/assets/index.md
  ...
}|

A way to remember this is that as the user, you are the first @racket[dependent] and your
dependencies are therefore subject to clarification.
}

@defproc[(compile-all-unlike [clarify (-> string? clear/c)]
                             [resolve (-> clear/c unlike-asset)]
                             [entries (listof string?)])
                             dict?]{
Like @racket[compile-unlike], except multiple entry points are accepted and
each entry maps to a thread that calls @racket[compile-unlike]. Blocks until
all threads terminate.

Returns a dictionary such that keys are the elements in @racket[entries] and the values
are either:

@itemlist[
@item{The corresponding return value from @racket[compile-unlike].}
@item{Any @racket[exn] raised from the call to @racket[compile-unlike] that may explain why that particular compilation failed.}]

Each thread will identify themselves in the log using a prefix matching its corresponding element in @racket[entries].}

