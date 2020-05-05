#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    unlike-assets]]

@title{Default Command Line Interface}
@defmodule[unlike-assets/cli]

@defproc[(u/a-cli) any]{
Consumes @racket[current-command-line-arguments] to interact with
@racketmodname[unlike-assets/distributor] or
@racketmodname[unlike-assets/server]. Forwards log messages on the
@racket['unlike-assets] topic to STDOUT.

All positional arguments in the command line are forwarded to
@racket[procure] before anything else happens. Each argument is logged
as an info-level event when the associated value is @racket[procure]d.

Only one of the following flags can be specified at a time.
Each flag is shown with the procedure applied via the flag's use.

@itemlist[

@item{@litchar{--serve, -s}: Calls @racket[start-server].}

@item{@litchar{--distribute, -d}: Calls
@racket[sync-filesystem-to-resolver-cache!].  By default, no files are
written. File activity is instead logged as info-level events. Use
@litchar{--no-dry-run} to modify the disk.}

]

The server or distributor will start after all assets named in
the command line are procured.

@itemlist[

@item{@litchar{--verbose, -v}: Includes debug-level log messages in
STDOUT.}

@item{@litchar{--port, -p}: (When @litchar{--server} is set) Selects the TCP port used to listen for connections.}

@item{@litchar{--no-dry-run, -n}: (When @litchar{--distribute} is set) Commits the actions of the distributor to disk.}

]

}