#lang scribble/manual

@title{Unlike Assets}

@defmodule[u/a #:lang]

The @litchar{u/a} language provides all bindings from
@racketmodname[racket/base], @racketmodname[racket/runtime-path], and
@racketmodname[unlike-assets/resolver]. It is meant to act as a runtime
configuration language for the Unlike Assets package ecosystem.

The modules created by @litchar{#lang u/a} can be used as a
command-line launcher that starts a development server or a
distribution process.

@racketmod[u/a

(define-runtime-paths (files-in files-out) "./assets" "./dist")
(require unlike-assets/racket-modules)
(u/a (racket-modules files-in))
]
