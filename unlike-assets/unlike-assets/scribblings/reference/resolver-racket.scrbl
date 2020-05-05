#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/rerequire
                    unlike-assets]]

@title{Racket Modules as Resolved Values}
@defmodule[unlike-assets/resolver/racket]

@defthing[module-path/c
         (or/c module-path? resolved-module-path? module-path-index?)]{
Captures values that @racket[racket-modules] can translate to a proper Racket
module path.
}

@defproc[(racket-modules [key->maybe-module-path (-> any/c (or/c #f module-path/c))]
                         [make-result (-> module-path/c (not/c procedure?)) module-path->hasheq])
                         (-> (or/c #f (-> (not/c procedure?))))]{
Returns a procedure @racket[P] suitable for use in @racket[replace-resolver].

@racket[P] consults @racket[key->maybe-module-path] to derive a usable
module path for both @racket[dynamic-rerequire] and
@racket[dynamic-require]. If @racket[key->maybe-module-path] returns
@racket[#f], then @racket[P] will yield to other
extensions. Otherwise, it will bind a given resolver key a procedure
that applies @racket[make-result] to the returned path.

The modules will not reload if you've already instantiated them
without a leading @racket[dynamic-rerequire]. In practice, you should
only use @racket[racket-modules] to manage Racket modules that do not
benefit from caching, or do not need to exist in a prototyping
context.
}

@defproc[(module-path->hasheq [module-path module-path/c]) hash-eq?]{
Loads the given Racket module using @racket[dynamic-require], and
populates a hash with all runtime exports of the module.
}
