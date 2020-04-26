#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
                    unlike-assets]]

@title{@tt{unlike-assets/distributor}}
@defmodule[unlike-assets/distributor]

Use this module to synchronize a file system with a resolver's cache,
optionally under a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{security guard}
that can enforce dry runs.

@defproc[(write-asset-to-filesystem!
         [to-filesystem distributable?]
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
         [R resolver? (current-resolver)]
         [#:exists exists symbol? 'error]) void?]{
Apply @racket[write-asset-to-filesystem!] to all compatible values in
@racket[R]. This is useful for saving a project to disk.

@racket[exists] functions as it does for @racket[open-output-file],
for each applicable file.
}

@defproc[(sync-filesystem-to-assets! [R resolver? (current-resolver)]) void?]{
Performs a one-way sync from @racket[R]'s cache to the host
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
