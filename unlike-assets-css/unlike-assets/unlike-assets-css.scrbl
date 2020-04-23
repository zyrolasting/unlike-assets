#lang scribble/manual

@title{Unlike Asset: Cascading Style Sheets (CSS)}

@require[@for-label[racket/base]]
@defmodule[unlike-assets/css]

This module produces CSS stylesheets as living assets.

@section{Racket-CSS Pidgin Language}
@defmodule[unlike-assets/css #:reader]

When used with @litchar{#reader}, @racketmodname[unlike-assets/css]
reads an expression that evaluates to a directory path, then a
sequence of @racketmodname[css-expr] expressions until the
@racket[#:end] keyword.

@racketblock[
(define body-width '600px)
(define styles #reader unlike-assets/css maybe-output-dir

[* #:box-sizing border-box]
[body #:width ,body-width
      #:background-image (apply url ,(procure/href "bg.webp"))]

#:end)

(displayln (styles))
]

In this example, @racket[styles] is bound to a thunk that returns a
CSS string. While the thunk is running, @racket[procure/href] assumes
that the given styles will appear in a stylesheet located at
@racket[maybe-output-dir]. If @racket[output-dir] is @racket[#f], then
@racket[procure/href] will behave like @racket[procure/weak/href].
This makes @racket[styles] non-deterministic, but the output styles
will always contain correct paths.
