#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    racket/function
                    unlike-assets]]

@title{Writing Resolver Extensions}
@defmodule[unlike-assets/resolver/extension]

This module reprovides @racketmodname[hash-partition],
@racketmodname[racket/contract], and @racketmodname[racket/hash].

A resolver extension does not have a defined interface, but
they can benefit from @tech[#:doc '(lib
"hash-partition/scribblings/hash-partition.scrbl")]{hash partitions}
or the caching patterns defined here.

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

@defform[(fenced-factory fence-expr factory-expr)]{

Combines @racket[make-factory-thunk] and @racket[make-fence-thunk].

This...

@racketblock[
(fenced-factory (file-or-directory-modify-seconds path)
                (file->string path))]

...expands to this.

@racketblock[
(make-factory-thunk
  (make-fence-thunk (lambda () (file-or-directory-modify-seconds path)))
  (lambda () (file->string path)))]

The expanded expression returns a procedure. When applied, that
procedure will apply @racket[file-or-directory-modify-seconds], and
then only apply @racket[file->string] when the modification time has
changed.

By the below example, @racket[(procure "my-file.txt")] will return the
latest contents of @tt{my-file.txt} in the working directory. The file
will only be read if the modification time has changed, or was not
recorded. References to old contents are discarded.

@racketblock[
(current-resolver
  (make-resolver
    (lambda (key dependents R)
      (define path
        (with-handlers ([exn:fail? (const #f)])
          (build-path (current-directory) key)))
      (and path
           (file-exists? path)
           (fenced-factory (file-or-directory-modify-seconds path)
                           (file->string path))))))]

}
