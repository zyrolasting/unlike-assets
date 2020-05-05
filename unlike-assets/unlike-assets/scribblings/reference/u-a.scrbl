#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    unlike-assets]]

@title{Configuration Language}

@(defmodulelang* (u/a))

The @tt{u/a} configuration language is
@racketmodname[racket/base] with all bindings from
@racketmodname[unlike-assets] and
@racketmodname[racket/runtime-path]. Most will use
@racket[replace-resolver] and the bindings from
@racketmodname[unlike-assets/config].
