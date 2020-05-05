#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    unlike-assets
                    unlike-assets/cli]]

@title{Command Line Interfaces}
@defmodule[unlike-assets/cli]

@defproc[(u/a-cli [run-action (-> string? any) (lambda (action) ...)]) any]{
Consumes @racket[current-command-line-arguments] to interact with
systems built using @racket[unlike-assets].

Assuming @racket[run-action] does nothing, @racket[u/a-cli] will only
install handlers to handle user breaks and forward log messages on the
@racket['unlike-assets] topic to STDOUT. It will only consume a
@litchar{--verbose, -v} flag to control whether STDOUT will include
debug-level messages.

@racket[u/a-cli] consumes the first command line argument after any
consumed flags. It will then apply @racket[run-action] to that
argument for its side-effect, in a parameterization where
@racket[current-command-line-arguments] is set to whatever
@racket[u/a-cli] did not consume.

This means that if you run @litchar{racket cli.rkt -v foo bar -a baz},
@racket[u/a-cli] will evaluate @racket[(run-action "foo")] with
@racket[(current-command-line-arguments)] set to @racket['#("bar" "-a"
"baz")]. You can use this to define your own subcommands without
giving up visible logging and error handling.

By default, @racket[run-action] is:

@racketblock[
(define (default-run-action action)
  ((case action
    [("distribute") u/a-cli/distribute]
    [("serve") u/a-cli/serve]
    [else (λ (r)
            (eprintf "Unknown action: ~a~n" action)
            (exit 1))])
   (current-resolver)))
]
}

@defproc[(u/a-cli/distribute [resolver resolver?]) any]{
Consumes @racket[current-command-line-arguments]
to call @racket[(sync-filesystem-to-resolver-cache! resolver)].

All positional arguments in the command line are forwarded to
@racket[resolver] before synchronizing the file system. Each argument
is logged as an info-level event when the associated value is
@racket[procure]d.

If @litchar{--no-dry-run, -n} is set, @racket[dry-run-enabled] is
first set to @racket[#f]. This will commit all changes to disk.
}

@defproc[(u/a-cli/serve [resolver resolver?]) any]{
Consumes @racket[current-command-line-arguments]
to call @racket[(start-server resolver #:port port)].

@litchar{--port, -p} determines the value of @racket[port]
in that call.
}
