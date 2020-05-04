#lang scribble/manual

@require[@for-label[racket/base
                    racket/file
                    racket/contract
                    unlike-assets
                    unlike-assets/resolver/extension]]

@title{Extending Resolvers}
@defmodule[unlike-assets/resolver/extension]

This module reprovides @racketmodname[hash-partition],
@racketmodname[racket/contract], and @racketmodname[racket/hash].

A resolver extension does not have a defined interface. It
does, however, have at least one of the following goals:

@itemlist[
@item{Edit keys passed to a resolver.}
@item{Add a @racket[key->thunk] procedure for a resolver.}
@item{Add data to a resolved value.}
@item{Use data in a resolved value.}
]

The first two goals can be met by simply wrapping procedures or values
before applying a resolver. Caching can be a concern, so this module
provides @racket[fenced-factory] as a caching pattern that works well
with resolvers.

Built-in extensions meet the latter two goals with @tech[#:doc '(lib
"hash-partition/scribblings/hash-partition.scrbl")]{hash partitions}.
You are not required to use hashes for your own extensions, but you
must use extensions as per their instructions.

@defproc[(make-fence-thunk [make (-> any/c)]
                           [same? (-> any/c any/c any/c) equal?]
                           [initial any/c #f]) boolean?]{
Returns a thunk. The thunk returns @racket[(not (same? prev (make)))],
where @racket[prev] is either @racket[initial] or the value of a prior
@racket[(make)].

Goes well with @racket[(make-factory-thunk)].

}

@defproc[(make-factory-thunk [make? (-> any/c)] [make (-> any/c)]) (-> any/c)]{
Returns a thunk that optionally caches a value before returning the
value in the cache.

When applied, the thunk will apply @racket[make] to update the cache
if the cache has never been updated, or if @racket[(make?)]
is a true value.

This implies that if @racket[(make)] aborts before updating the cache
for the first time, a later application of the thunk will apply
@racket[make] again regardless of the value of @racket[(make?)].

Goes well with @racket[(make-fence-thunk)]. A factory thunk is useable
as a return value for @racket[key->thunk] in a @tech{resolver}.
}

@defform[(fenced-factory fence factory)]{
This...

@racketblock[
(fenced-factory (file-or-directory-modify-seconds path)
                (file->string path))]

...expands to this.

@racketblock[
(make-factory-thunk (make-fence-thunk (lambda () (file-or-directory-modify-seconds path)))
                    (lambda () (file->string path)))]

The behavior of this procedure is to apply
@racket[file-or-directory-modify-seconds], and then only apply
@racket[file->string] when the cached modification time has changed.

Here's an example of how to install a fenced factory using @racket[replace-resolver].

@racketblock[
(replace-resolver
  (lambda (key R)
    (define path
      (with-handlers ([exn:fail? (const #f)])
        (build-path (current-directory) key)))
    (and path
         (file-exists? path)
         (fenced-factory (file-or-directory-modify-seconds path)
                         (file->string path)))))]

Once installed, every application of @racket[(procure "my-file.txt")]
will return the latest contents of @tt{my-file.txt} in the current
working directory. The file will only be read if the modification
time has changed, and references to old contents are discarded.
If a key does not map to an existing file, then the resolver will
claim that there is no value to resolve.
}
