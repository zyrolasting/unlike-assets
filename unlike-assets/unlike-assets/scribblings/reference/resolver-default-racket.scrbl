#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/rerequire
                    unlike-assets/resolver
                    unlike-assets/resolver/default/racket]]

@title{@tt{unlike-assets/resolver/default/racket}}
@defmodule[unlike-assets/resolver/default/racket]

@defthing[module-path/c
         (or/c module-path? resolved-module-path? module-path-index?)]{
Captures values that @racket[racket-modules] can translate to a proper Racket
module path.
}

@defproc[(racket-modules [make-module-path (-> string? (or/c #f module-path/c))]
                         [make-result (-> module-path/c any/c) module-path->hasheq])
                         (-> (or/c #f procedure?))]{
Returns a procedure suitable for use in @racket[replace-resolver].

The procedure consults @racket[make-module-path] to derive a usable
module path for both @racket[dynamic-rerequire] and
@racket[dynamic-require]. If @racket[make-module-path] returns
@racket[#f], then the procedure will yield to other
extensions. Otherwise, it will return a procedure that
applies @racket[make-result] to the returned path.

The modules will not reload if you've already instantiated them
without a leading @racket[dynamic-rerequire], so you should only use
@racket[racket-modules] to manage Racket modules with reload support.
}

@defproc[(module-path->hasheq [module-path module-path/c]) hash-eq?]{
Loads the given Racket module using @racket[dynamic-require], and
populates a hash with all runtime exports of the module.
}
