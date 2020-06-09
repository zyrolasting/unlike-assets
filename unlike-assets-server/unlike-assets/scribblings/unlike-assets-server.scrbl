#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/exn
                    racket/string
                    racket/tcp
                    net/url
                    web-server/web-server
                    web-server/http/request-structs
                    web-server/http/response-structs
                    web-server/dispatchers/dispatch
                    unlike-assets]]

@title{Unlike-Assets: Server}
@defmodule[unlike-assets/server]

Use this module to build a server that interacts with
@tech{seats}. @racketmodname[unlike-assets/server] reprovides all
bindings from @racketmodname[web-server/http/request-structs] and
@racketmodname[web-server/http/response-structs].


@defstruct*[(serveable [make-response (or/c response? (-> request? response?))])]{

A structure used as a resolved value by the server.
}


@defproc[(make-dispatcher-with-seat [seat seat/c]) dispatcher/c]{

Creates a dispatcher that responds to requests using a @tech{seat}.

If a @tech{resolved value} is @racket[serveable?], then the dispatcher
will use it to build a response.  Otherwise the server will respond
with a @racket[pretty-print]ed view of the @tech{resolved value} as a
@racket[#"text/plain; charset=utf-8"] body.

If an error is raised while fulfilling the request, then the
dispatcher will respond with a 500 status code and a plain text body
showing the result of @racket[exn->string].

This dispatcher does not use @racket[next-dispatcher], so it should
appear at the end of any dispatcher sequence.

}


@defproc[(start-seat-server [seat seat/c] [#:port port listen-port-number?]) procedure?]{

Starts a server using @racket[(make-dispatcher seat)] on the given
port.

Returns a procedure that, when applied, stops the server.

}
