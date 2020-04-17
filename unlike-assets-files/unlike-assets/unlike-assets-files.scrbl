#lang scribble/manual

@title{@tt{Unlike Asset: File}}

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    unlike-assets/resolver
                    unlike-assets/files
                    unlike-assets/files/distributor]]

@defmodule[unlike-assets/files]

@racketmodname[unlike-assets/files] extends
@racket[unlike-assets/resolver] to treat files as assets.

@section{Asset Definitions}
@defthing[asset/file-sourced/c (and/c (asset/c asset/readable/c [input-file-path complete-path?]))]{
Confirms if an asset can be read from a file system.
}

@defthing[asset/file-destined/c (and/c asset/writeable/c (asset/c [output-file-path complete-path?]))]{
Confirms if an asset can be written to a file system.
}

@defthing[asset/file-to-file/c (and/c asset/file-sourced/c asset/file-destined/c)]{
You get the idea.
}

@section{Resolving Filesystem Paths}

@defmodule[unlike-assets/files/resolve]

(define (find-file-path [#:must-exist must-exist boolean? #t]
                        [p (or/c path-for-some-system? path-string?)])
                        [search-dirs (non-empty-listof complete-path?) (list (current-directory))]
                        (or/c #f complete-path?)]{
If @racket[p] is complete, this returns @racket[p].  Otherwise, this
returns the first path built from each directory in
@racket[search-dirs] to refer to an existing file.

If no file is found, the result depends on @racket[must-exist].  If
@racket[must-exist] is a true value, this will raise
@racket[exn:fail:filesystem]. Otherwise, the result is @racket[#f].
}

@section{Integrating with the Asset Resolver}
@defproc[(file->asset [p complete-path?]) asset/file-sourced/c]{
Returns an asset that represents a file.
}

@defproc[(make-key->live-build [search-dirs (non-empty-listof complete-path?) (list (current-directory))])
         procedure?]{
Returns a procedure @racket[P] suitable for use in @racket[current-key->live-build].

@racket[(P key sys)] returns @racket[#f] if the key is not applicable
as a path or path string for some filesystem.

Otherwise, it returns a living build that maintains the latest value
of @racket[(file->asset (find-file-path search-dirs))].

Change detection is based on
@racket[file-or-directory-modify-seconds], which means that the asset
can be up to one second out of date.
}
