#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    unlike-assets]]

@title{@tt{unlike-assets/resolver/default/file}}
@defmodule[unlike-assets/resolver/default/file]


@defproc[(show-file-info [path (and/c complete-path? file-exists?)]) hash-eq?]{
Returns:

@racketblock[
(hasheq 'file-or-directory-identity
        (file-or-directory-identity path)
        'file-or-directory-permissions
        (file-or-directory-permissions path)
        'file-size
        (file-size path)
        'file-or-directory-modify-seconds
        (file-or-directory-modify-seconds path))]

Useful for verifying a @racket[file-modules] configuration.
}

@defproc[(file-modules [on-changed-file (-> (and/c complete-path? file-exists?) hash-eq?)]
                       [key->path (-> any/c (or/c #f complete-path?))])
                       (-> any/c (or/c #f (-> hash-eq?)))]
         procedure?]{
Returns a procedure @racket[P] suitable for use in @racket[replace-resolver].

@racket[P] will try to convert a resolver key to a complete path using
@racket[key->path].  If @racket[key->path] returns @racket[#f], then
@racket[P] returns @racket[#f].  It also returns @racket[#f] if
@racket[key->path] returns a path to a non-existant file.

When a existing file is found, the key is bound to a thunk
that monitors the file using @racket[file-or-directory-modify-seconds].
If the file has changed, then the thunk applies @racket[on-changed-file]
to the path. If the file is removed, then it will raise the appropriate
error.
}
