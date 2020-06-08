#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    unlike-assets]]

@title{Filesystem Resolver}
@defmodule[unlike-assets/resolver/file]

@defproc[(make-filesystem-resolver
           [#:search-directories search-directories
            (non-empty-listof (or/c path-string? path-for-some-system?))
            null]
           [#:dependent-relative? dependent-relative? any/c #t]
           [#:include-directories? include-directories? any/c #f]
           [#:expand-path expand-path (or/c #f (-> path? complete-path?)) #f]
           [#:changed? changed? (-> complete-path? any/c) file-or-directory-modify-seconds]
           [make-value (-> complete-path? any/c)])
         resolver/c]{
Returns a @tech{resolver} that reasons about paths, files, directories, and links.

This resolver returns complete paths as @tech{resolved names}. It will
raise @racket[exn:fail:unlike-assets:unresolved] if an
@tech{unresolved names} is not a @racket[path-string?], or if a
complete path is forbidden according to the configuration.

@racket[directories], @racket[files], and @racket[links] all
accept the following symbols:

@itemlist[

@item{@racket['must-exist]: The complete path must point to an existing entry in the file system to count as a resolved name.}

@item{@racket['disallow]: }

]

If a user requests a complete path, then the resolver will use a
simplified form of that path as the @tech{resolved name} if the
configuration allows that path.

If the user requests a relative path, then the resolver will assign a
base path according to a few rules. If there is a dependent file and
@racket[dependent-relative?] is true, then path is relative to the
dependent file's directory. Otherwise, if @racket[search-directories]
is not empty, then search for the first allowed path in that list.

The resolver computes a value by applying @racket[make-value] to the
complete path used as the @tech{resolved name}. Subsequent calls apply
@racket[changed?] to the same path. If @racket[changed?] returns a
true value, then @racket[make-value] is applied again.

]

}
