#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
                    net/url
                    web-server/web-server
                    web-server/http/request-structs
                    web-server/http/response-structs
                    web-server/dispatchers/dispatch
                    unlike-assets/resolver
                    unlike-assets/server]]

@title{Unlike Assets Server}

@defmodule[unlike-assets/server]

This module helps you set up a server that forwards requests to the
asset resolver.

@defthing[asset/serveable/c (asset/c [->http-response (-> request? response?)])]{
Confirms that an asset can be used to compute an HTTP response.
}

@defproc[(default-url->asset-key [u url?]) string?]{
Equivalent to @racket[(string-join (map path/param-path (url-path u)) "/")].
}

@defproc[(make-dispatcher [url->asset-key (-> url string?)]) dispatcher/c]{
Creates a dispatcher that maps any request @racketfont{R} to the
response provided by a procured asset.  The key used to procure the
asset is first created using @racket[(url->asset-key (request-uri
R))].


If an error is thrown while fulfilling the request, then the
dispatcher will respond with a 500 status code and the captured
content from @racket[error-display-handler] as a @racket[#"text/plain; charset=utf-8"] body.

If the asset matches @racket[asset/serveable/c], then the server
will respond with the output of it's @racket['->http-response] key.
Otherwise the server will respond with formatted view of the
asset's underlying hash as a @racket[#"text/plain; charset=utf-8"] body.

This dispatcher does not use @racket[next-dispatcher], so it should
appear at the end of any dispatcher sequence.
}

@defproc[(start-server [url->asset-key (-> url? string?) default-url->asset-key]
                       [port exact-positive-integer?]) procedure?]{
Starts a development server using @racket[(make-dispatcher url->asset-key)]
on the given port.

Returns a procedure that, when applied, stops the server.
}
