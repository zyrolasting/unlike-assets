#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/string
                    unlike-assets]
                    "macros.rkt"]

@title{Writing Resolved Values to Disk}
@defmodule[unlike-assets/distributor]

Use this module to synchronize a file system with a resolver's cache,
optionally under a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{security guard}
for dry runs.

@defthing[dry-run-enabled (parameter/c boolean?) #:value #t]{
When @racket[#t] (default), any destructive operation in this module
will start by setting @racket[current-security-guard] to one that
blocks writes, deletions, and executions of files. It will also block
all network activity. All destructive operations on files are
considered successful, and the operation is logged for the
@racket['unlike-assets] topic on the @racket['info] level.

Due to how security guards work, setting @racket[dry-run-enabled]
to @racket[#f] within a parameterization that set it to @racket[#t]
will have no effect. You must set it to @racket[#f] before applying
any of the destructive procedures herein.
}

@defhashpartition[(distributable [path complete-path?]
                                 [write-file (-> output-port? any/c)])]{
A @tech[#:doc '(lib "hash-partition/scribblings/hash-partition.scrbl")]{hash partition}
that holds the data needed to export the complete contents of a file to a given path.
}

@defproc[(write-distributable-to-filesystem! [to-filesystem distributable?])
         any/c]{
Equivalent to @racket[((distributable-write-file to-filesystem)
to-file)], where @racket[to-file] is an output port to the file located
at @racket[(distributable-path to-filesystem)].

If the file exists, it will be replaced.

Use @racket[dry-run-enabled] to prevent unwanted writes.

Returns the result of @racket[distributable-write-file] as defined by
that distributable.
}

@defproc[(write-resolved-to-filesystem!
         [R resolver? (current-resolver)])
         (hash/c complete-path? any/c)]{
Applies @racket[write-distributable-to-filesystem!] to all
@racket[distributable?] values in @racket[(R)]. This is useful for
saving a project to disk.

Returns a hash of paths to the output of corresponding return values
for @racket[write-distributable-to-filesystem!].
}

@defproc[(sync-filesystem-to-resolver-cache! [R resolver? (current-resolver)]) void?]{
Performs a one-way sync from @racket[R]'s cache to the host
filesystem. The post-condition is every directory containing files
written by this procedure will contain @italic{only} those files.

Like all filesystem syncs, the post-condition is either helpful or
disastorous. A @racket[distributable?] that defines itself as
belonging to a sensitive directory will cause this procedure to delete
content in that directory.  Lean on @racket[dry-run-enabled] until you
are comfortable with the consequences, and be wary of the OS-level
permissions set on the process.
}
