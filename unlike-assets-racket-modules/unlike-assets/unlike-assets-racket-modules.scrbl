#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/rerequire
                    unlike-assets/racket-modules
                    unlike-assets/resolver]]

@title{Unlike Asset: Racket Module}

@defmodule[unlike-assets/racket-modules]

@deftogether[(
@defthing[module-path/c (or/c module-path? resolved-module-path? module-path-index?)]
@defproc[(racket-modules [make-module-path (or/c path-for-some-system?
                                                 path-string?
                                                 (-> string? module-path/c))]
                         [user-data any/c] ...)
                         procedure?])]{
Returns a procedure @racket[P] suitable for use in @racket[u/a].

@racket[P] consults @racket[make-module-path] to derive a usable module
path for @racket[dynamic-rerequire] and @racket[dynamic-require].

@itemlist[
@item{If @racket[make-module-path] is a path for a filesystem, then it
will be treated as a directory. Any request for a module using a
relative path will be relative to that directory. If the directory
does not exist or is not readable, then an argument error will be
raised.}

@item{If @racket[make-module-path] is a procedure, then it must
map a string key from @racket[current-u/a-build-system] to a module path.}]

The module, when loaded, will always produce an asset by evaluating
@racket[(apply (dynamic-require module-path 'make-asset) user-data)].

There are some caveats:

@itemlist[
@item{Requested modules will not reload if you've already instantiated
them without a leading @racket[dynamic-rerequire]. You should only use
@racket[racket-modules] to manage Racket modules that you do not plan
to load any other way.}

@item{Some @racket[u/a] extensions require you to pass
@racket[user-data] that meets a particular contract. You can manage
any overlap using multiple calls to @racket[racket-modules].}]}
