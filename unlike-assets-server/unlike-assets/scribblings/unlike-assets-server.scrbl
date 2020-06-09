#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/exn
                    racket/string
                    racket/pretty
                    racket/tcp
                    net/url
                    web-server/web-server
                    web-server/http/request-structs
                    web-server/http/response-structs
                    web-server/dispatchers/dispatch
                    unlike-assets
                    unlike-assets/server]]

@title{Unlike-Assets: Server}
@defmodule[unlike-assets/server]

@(define (u/a-tech str)
   (tech #:doc '(lib "unlike-assets/scribblings/reference/unlike-assets-reference.scrbl") str))

@(define resolved-value (u/a-tech "resolved value"))

Use this module to build a server that interacts with
@u/a-tech{seats}. @racketmodname[unlike-assets/server] reprovides all
bindings from @racketmodname[web-server/http/request-structs] and
@racketmodname[web-server/http/response-structs].

@defproc[(make-dispatcher-with-seat
          [#:on-error on-error (-> request? exn? response?) (lambda () ...)]
          [#:on-other on-other (-> request? any/c response?) (lambda () ...)]
          [seat (seat/c any/c)])
          dispatcher/c]{

Creates a dispatcher that responds to requests using a @u/a-tech{seat}.

@racket[request] instances are used as @u/a-tech{unresolved names}.

The @u/a-tech{resolved names} depend on the @u/a-tech{resolver}, which
in turn impacts caching and dependency resolution.

If a @resolved-value is a @racket[response], then the dispatcher will
use that response. Otherwise, the dispatcher will apply
@racket[on-other] to the value. By default, @racket[on-other]
@racket[pretty-print]s the @resolved-value in a @racket[#"text/plain;
charset=utf-8"] response body.

If an @racket[exn] is raised while fulfilling the request, then the
dispatcher will respond with @racket[on-error]. By default,
@racket[on-error] returns a @racket[response] with a @racket[500]
status code and @racket[#"text/plain; charset=utf-8"] body showing the
output of @racket[exn->string].

}


@defproc[(serve-seat
          [#:on-error on-error (-> request? exn? response?) (lambda () ...)]
          [#:on-other on-other (-> request? any/c response?) (lambda () ...)]
          [#:port port listen-port-number?])
          procedure?]{

Starts a server using @racket[make-dispatcher-with-seat].
The related arguments and defaults are the same.

Returns a procedure that stops the server when applied.

}
