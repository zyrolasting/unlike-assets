#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/rerequire
                    unlike-assets]]

@title{Racket Module Resolver}
@defmodule[unlike-assets/resolver/racket]

@defthing[module-path/c
         (or/c module-path? resolved-module-path? module-path-index?)]{
Captures values that @racket[racket-modules] can translate to a proper Racket
module path.
}

@defproc[(make-racket-module-resolver
          [make-module-path (-> any/c list? module-path/c)]
          [make-result (-> module-path/c list? seat/c any/c) (lambda () ...)]
          [#:verbosity verbosity (or/c 'all 'reload 'none)]
          [#:reload? reload? any/c #t])
          resolver/c]{
Returns a resolver that reasons about Racket modules and module paths.

The resolver only accepts @tech{unresolved names} that match @racket[module-path?].

@tech{resolved names} are @racket[module-path/c] values, which are later
used in @racket[dynamic-require]. @racket[make-result] must create a
single value to represent the module. By default, that value will be
@racket[(module-path->hasheq module-path 0)].

If @racket[reload?] is true, then the target module will first be
loaded using @racket[dynamic-rerequire] at the given
@racket[verbosity].  Each subsequent attempt to request a Racket
module via a @tech{resolver} will therefore return the latest value
from @racket[make-result].

@bold{Clarifications:}

@itemlist[

@item{The modules will not reload if you've already instantiated them
without a leading @racket[dynamic-rerequire].}

@item{You can use an isolated filesystem resolver (See
@racket[make-filesystem-resolver]) to compute in
@racket[make-module-path].}

]

}

@defproc[(module-path->hasheq [module-path module-path/c] [phase exact-nonnegative-integer? 0]) hash-eq?]{
Loads the given Racket module using @racket[dynamic-require], and
populates a hash with all exports of the module at a given phase
(@racket[0] being runtime).
}
