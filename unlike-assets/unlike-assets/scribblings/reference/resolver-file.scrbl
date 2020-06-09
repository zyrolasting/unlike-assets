#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    unlike-assets]]

@title{Filesystem Resolver}
@defmodule[unlike-assets/resolver/file]

@defproc[(make-filesystem-resolver
           [#:search-directories search-directories
            (non-empty-listof (or/c path-string? path-for-some-system?))]
           [#:dependent-relative? dependent-relative? any/c #t]
           [#:must-exist? must-exist? any/c #t]
           [#:allow-directories? allow-directories? any/c #f]
           [#:allow-files? allow-files? any/c #t]
           [#:allow-links? allow-links? any/c #f]
           [#:rewrite-path rewrite-path (-> path-string? list? path-string?) (lambda (p deps) p)]
           [#:changed? changed? (-> complete-path? any/c) file-or-directory-modify-seconds]
           [make-value (-> complete-path? any/c)])
         resolver/c]{
Returns a @tech{resolver} that reasons about paths, files, directories, and links.

This resolver returns complete simple paths as @tech{resolved
names}. It will raise @racket[exn:fail:unlike-assets:unresolved] if an
@tech{unresolved name} is not a @racket[path-string?], or if a
complete path is forbidden according to the configuration.

The resolver computes a value by applying @racket[make-value] to the
complete path used as the @tech{resolved name}. Subsequent calls apply
@racket[changed?] to the same path. If @racket[changed?] returns a
true value, then @racket[make-value] is applied again.

Before carrying out any of the below rules regarding name resolution,
the value representing the @tech{unresolved name} is first transformed
to a new path using @racket[rewrite-path]. @racket[rewrite-path] must
accept a path (or a string representing a path), and a
@tech{dependents list}.

Use this to support custom expansions,
such as @tt{~/file.txt} to @tt{/path/to/assets/file.txt}.

If a user requests a complete path, then the resolver will use that
path as the @tech{resolved name} if the configuration allows it.

If @racket[must-exist?] is true, then @racket[allow-files?],
@racket[allow-directories?], and @racket[allow-links?] each control if
a complete path may refer to the respective resource. If
@racket[must-exist?] is @racket[#f], then the paths are not checked
in this way.

If the user requests a relative path, then the resolver will assign a
base path according to a few rules. If there is a dependent file and
@racket[dependent-relative?] is true, then base path is the complete
path to the dependent file's directory. Otherwise, if
@racket[search-directories] is not empty, then the base path
is the first path allowed in that list.

@bold{Other clarifications:}

@itemlist[

@item{The default settings are for a resolver that searches only for
existing files, and computes a new value when the modification time of
the target file has changed. You must at least specify
@racket[search-directories] to handle the case of paths without
dependents.  If a file is no longer accessible from an existing
location, then the thunk created by the resolver will raise a
relevant exception on its next use.}

@item{If the rules are too messy or unmanageable, consider setting
@racket[rewrite-path] to only return complete paths. This will remove
all casework for relative paths and allow you to control how paths are
represented in your project. Alternatively, use multiple filesystem
resolvers with simpler configurations and combine them with
@racket[rcons] or @racket[rlist].}

@item{If @racket[must-exist?] is @racket[#f] and
search directories are used, then the first possible search path is
used as the @tech{resolved name}.}

@item{@racket[must-exist?] only applies to name resolution.
If a file, directory, or link gets deleted during use,
then only @racket[changed?] will detect that.}

@item{If there is a dependent and its @tech{resolved
name} is not a complete path, then dependents will not be considered
when computing a base path.}

]

}
