#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    unlike-assets]]

@title{Files as Resolved Values}
@defmodule[unlike-assets/resolver/file]

@defproc[(get-file-info [path (and/c complete-path? file-exists?)]) hash-eq?]{
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

Useful for verifying an @racket[existing-files] configuration.
}

@defproc[(search-within [search-dirs (or/c (or/c path-string?
                                                 path-for-some-system?)
                                     (non-empty-listof (or/c path-string?
                                                             path-for-some-system?)))]
                        [match? (-> (or/c path-string?
                                          path-for-some-system?)
                                     any/c)
                                file-exists?])
                        (-> (or/c path-string? path-for-some-system?) (or/c #f complete-path?))]{
Returns a procedure @racket[search].

@racket[(search relative-path)] returns the first complete path
@racket[P] where @racket[(match? P)] is true, or @racket[#f] if no
path matches. The candidate paths are built using
@racket[search-dirs].
}

@defproc[(existing-files [on-changed-file (-> (and/c complete-path? file-exists?) (not/c procedure?))]
                         [key->maybe-complete-path (-> any/c (or/c #f complete-path?))])
                         (-> any/c (or/c #f (-> (not/c procedure?))))]{
Returns a procedure @racket[P] suitable for use in
@racket[replace-resolver] as an extension. @racket[P] represents a
set of existing files.

@racket[P] will try to convert a resolver key to a complete path using
@racket[key->maybe-complete-path].  If
@racket[key->maybe-complete-path] returns @racket[#f], then @racket[P]
returns @racket[#f]. Otherwise, @racket[key->maybe-complete-path]
returns a complete path to a presumably existing file.

This extension will respond to requests for files by checking a target
file using @racket[file-or-directory-modify-seconds].  If the file has
changed, then the thunk applies @racket[on-changed-file] to derive a
new value.  If the file is not accessible, then the extension will
raise the relevant error.
}
