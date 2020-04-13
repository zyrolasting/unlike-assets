#lang scribble/manual

@require[@for-label[net/url
                    web-server/web-server
                    web-server/http/request-structs
                    web-server/http/response-structs
                    unlike-assets/reactive
                    web-server/dispatchers/dispatch]]

@title{Unlike Assets Development Server}

@defmodule[unlike-assets/dev-server]

This module provides tools for setting up a development server that
forwards requests to the unlike asset resolver.

@defproc[(default-url->asset-key [u url?])]{
Equivalent to @racket[(string-join (map path/param-path (url-path u)) "/")].
}

@defproc[(make-dispatcher [url->asset-key (-> url string?)]) dispatcher/c]{
Creates a dispatcher that maps any request @racketfont{R} to the
response provided by a procured @racket[asset/serveable/c] asset.  The
key used to procure the asset is first created using
@racket[(url->asset-key (request-uri R))].

If an error is thrown while fulfilling the request, then the
dispatcher will respond with a 500 status code and the captured
content from @racket[current-error-display-handler] as a
@racket[#"text/plain; charset=utf-8"] body.

This dispatcher does not use @racket[next-dispatcher], so it should
appear at the end of any dispatcher sequence.
}

@defproc[(start-server [url->asset-key (-> url? string?) default-url->asset-key]
                       [port exact-positive-integer?]) procedure?]{
Starts a development server using @racket[(make-dispatcher url->asset-key)]
on the given port.

Returns a procedure that, when applied, stops the server.
}
