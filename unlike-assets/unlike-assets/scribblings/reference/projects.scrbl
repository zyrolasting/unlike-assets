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
                    unlike-assets/projects]]

@title{@tt{unlike-assets/projects}}

@defmodule[unlike-assets/projects]

@racketmodname[unlike-assets/projects] includes all bindings from
@racketmodname[unlike-assets/projects/server],
@racketmodname[unlike-assets/projects/cli], and
@racketmodname[unlike-assets/projects/distributor]. Altogether
they help you construct an interface for a resolver.

@section{@tt{Development Server}}
@defmodule[unlike-assets/projects/server]

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


@section{Distributing Assets to Filesystem}
@defmodule[unlike-assets/projects/distibutor]

Assets can control how they appear as files to some degree, but this
module has the last say. Use
@racketmodname[unlike-assets/projects/distibutor] to export assets as
files either individually, or in bulk. You can configure the
distributor to perform a dry run, during which a security guard
will block and log all deletes and writes.

@defproc[(write-asset-to-filesystem!
         [to-filesystem (and/c asset/with-write/c asset/file-destined/c)]
         [#:dry-run? boolean? #t]
         [#:exists exists symbol? 'error])
         void?]{
Write an asset's byte representation to it's intended destination. If
@racket[(to-filesystem 'output-file-path)] is a relative path, then
the resulting file will appear in its corresponding location in
@racket[(current-output-directory)].

@racket[exists] functions as it does for @racket[open-output-file].
}

@defproc[(write-resolved-to-filesystem!
         [sys resolver? (current-resolver)]
         [#:exists exists symbol? 'error]) void?]{
Apply @racket[write-asset-to-filesystem!] to all compatible assets
in @racket[sys]. This is useful for saving project distributions.

@racket[exists] functions as it does for @racket[open-output-file],
for each applicable file.
}

@defproc[(sync-filesystem-to-assets! [sys resolver? (current-resolver)]) void?]{
Performs a one-way sync from the assets in @racket[sys] to the host
filesystem.

Like all filesystem syncs, this is a destructive operation that may
entail a lot of disk activity. It also assumes that if any affected
directory contains files, those files represent out-of-date results
from the last application of this procedure. If this is not true, then
do not use this procedure. If you do use this procedure, lean on
@racket[dry-run?] until you are comfortable with the consequences.

These are the post-conditions:

@itemlist[
@item{The latest versions of all assets will exist on disk as files in their respective paths.}
@item{All files that meet all of these conditions are @bold{deleted}:
          @itemlist[
          @item{The file was NOT written by the call to @racket[sync-filesystem-to-assets!]}
          @item{The file is in a directory populated by the call.}]}
]
}
