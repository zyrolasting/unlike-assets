#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    racket/function
                    unlike-assets]]

@title{Thunks}
@defmodule[unlike-assets/resolver/thunk]

@racket[make-resolver] accepts a @racket[make-thunk] argument, which
obviously returns a thunk. This module offers bindings that help you
write thunks with custom caching and change detection rules.

@defthing[value-thunk/c (-> any/c)]{
A nullary procedure that returns a single value.
}

@defproc[(make-fence-thunk [make value-thunk/c]
                           [same? (-> any/c any/c any/c) equal?]
                           [initial any/c #f]) boolean?]{
Returns a thunk. The thunk returns @racket[(not (same? prev (make)))],
where @racket[prev] is either @racket[initial] or the value of a prior
@racket[(make)].

Goes well with @racket[make-factory-thunk].
}

@defproc[(make-factory-thunk [make? value-thunk/c] [make value-thunk/c]) value-thunk/c]{
Returns a thunk that optionally caches a value before returning the
value in the cache.

When applied, the thunk will apply @racket[make] to update the cache
if the cache has never been updated, or if @racket[(make?)]
is a true value.

This implies that if @racket[(make)] aborts before updating the cache
for the first time, a later application of the thunk will apply
@racket[make] again regardless of the value of @racket[(make?)].

Goes well with @racket[make-fence-thunk].
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

The below @tech{resolver} will return the latest contents of
@tt{my-file.txt} in the working directory. The file will only be read
if the modification time has changed, or was not recorded. References
to old contents are discarded.

@racketblock[
(make-resolver
  (lambda (unresolved-name dependents)
    (define (resolver-assert test)
      (or test
      (raise-name-resolution-error unresolved-name
                                   dependents)))

    (resolver-assert (path-string? unresolved-name))
    (define path (build-path (current-directory) unresolved-name))
    (resolver-assert (file-exists? path))
    path)

  (lambda (path dependents seat)
    (fenced-factory (file-or-directory-modify-seconds path)
                    (file->string path))))]
}
