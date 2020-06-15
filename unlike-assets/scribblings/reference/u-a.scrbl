#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/runtime-path
                    u/a]]

@title{Language}

@(defmodulelang* (u/a))

@racketmodname[u/a] provides all bindings from
@racketmodname[racket/base], @racketmodname[unlike-assets/config],
@racketmodname[unlike-assets/resolver], and
@racketmodname[unlike-assets/logging].

@litchar{#lang u/a} modules are a target for @racket[nearest-u/a].
You can opt-out of this behavior by using @racketmodname[u/a] in
@racket[require].
