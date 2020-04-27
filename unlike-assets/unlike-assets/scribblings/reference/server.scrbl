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
                    unlike-assets]
                    "macros.rkt"]

@title{@tt{unlike-assets/server}}
@defmodule[unlike-assets/server]

This module helps you set up a prototyping server that forwards
requests to a @tech{resolver}. This module reprovides all bindings
from @racketmodname[web-server/http/request-structs] and
@racketmodname[web-server/http/response-structs].

Do not use this server on production systems.

@defextension[serveable [make-response (-> request? response?)]]

@defproc[(make-dispatcher [R resolver?] [url->key (-> url any/c)]) dispatcher/c]{
Creates a dispatcher that responds to requests using @racket[serveable]
objects.

On any request, the request URI is translated to a resolver key using
@racket[url->key]. If the asset is @racket[serveable?], then the
server will respond using its response object or handler.  Otherwise
the server will respond with a @racket[pretty-print]ed view of the
result as a @racket[#"text/plain; charset=utf-8"] body.

If an error is thrown while fulfilling the request, then the
dispatcher will respond with a 500 status code and a plain
text body showing the result of @racket[exn->string].

This dispatcher does not use @racket[next-dispatcher], so it should
appear at the end of any dispatcher sequence.
}

@defproc[(start-server [R resolver?]
                       [url->key (-> url? string?) default-url->key]
                       [#:port port listen-port-number?])
                       procedure?]{
Starts a development server using @racket[(make-dispatcher R url->key)]
on the given port.

Returns a procedure that, when applied, stops the server.
}

@defproc[(default-url->key [u url?]) string?]{
Equivalent to @racket[(string-join (map path/param-path (url-path u))
"/")].  When used with @racket[make-dispatcher] or
@racket[start-server], a GET for @tt{/index.html?blah=1} is equivalent
to evaluating @racket[(R "index.html")].
}
