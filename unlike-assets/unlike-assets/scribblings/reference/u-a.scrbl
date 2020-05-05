#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    unlike-assets]]

@title{Configuration Language}

@(defmodulelang* (u/a))

The @tt{u/a} configuration language is @racketmodname[racket/base]
with all bindings from @racketmodname[unlike-assets] and
@racketmodname[racket/runtime-path]. You will most likely use
@racket[replace-resolver] and the bindings from
@racketmodname[unlike-assets/config].

Any module using @litchar{#lang u/a} is a search target for
@racket[nearest-u/a].
