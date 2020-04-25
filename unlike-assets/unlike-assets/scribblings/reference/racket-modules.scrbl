#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/rerequire
                    unlike-assets/racket-modules
                    unlike-assets/files
                    unlike-assets]]

@title{@tt{unlike-assets/racket-modules}}

@defmodule[unlike-assets/racket-modules]

@defthing[module-path-like/c
         (or/c path-for-some-system? module-path? resolved-module-path? module-path-index?)]{
Captures values that @racket[racket-modules] can translate to a proper Racket
module path.
}

@defproc[(racket-modules [make-module-path (-> string? (or/c #f module-path-like/c))]
                         [module-path->asset (-> module-path-like/c asset?) make-asset-from-provides])
                         route/c]{
Returns a procedure suitable for use as an extension in @racket[u/a].

@margin-note{If you want to scope this extension to a limited part of the
filesystem, consider using @racket[within-directories].}
The procedure consults @racket[make-module-path] to derive a usable
module path for @racket[dynamic-rerequire]. If
@racket[make-module-path] returns @racket[#f], then the procedure will
yield to other extensions. Otherwise, it will apply
@racket[module-path->asset] to the returned path.

The modules will not reload if you've already instantiated them
without a leading @racket[dynamic-rerequire], so you should only use
@racket[racket-modules] to manage Racket modules with reload support.
}

@defproc[(make-asset-from-provides [module-path (or/c module-path?
                                                      resolved-module-path?
                                                      module-path-index?)])
                                   asset?]{
Loads the given Racket module using @racket[dynamic-require], and
populates an asset with all runtime exports of the module.
}
