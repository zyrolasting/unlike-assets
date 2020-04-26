#lang scribble/manual

@require[@for-label[racket/base
                    unlike-assets]]

@title{@tt{u/a}}
@defmodule[@racketfont{u/a} #:lang #:module-paths (u/a/lang/reader)]

The @tt{u/a} configuration language is
@racketmodname[racket/base] with all bindings from
@racketmodname[unlike-assets] and
@racketmodname[racket/runtime-path]. Most will use
@racket[replace-resolver] and the bindings from
@racketmodname[unlike-assets/config].
