#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/runtime-path
                    unlike-assets]]

@title{Configuration Language}

@(defmodulelang* (u/a))

@racketmodname[u/a] provides all bindings from
@racketmodname[racket/base] and
@racketmodname[unlike-assets]. @litchar{#lang u/a} modules are a
target for @racket[nearest-u/a].
