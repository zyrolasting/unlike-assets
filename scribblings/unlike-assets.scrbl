#lang scribble/manual
@require[@for-label[unlike-assets racket/base]]

@title{Processing unlike assets}
@author{Sage Gerard}

@defmodule[unlike-assets]

This collection provides primitives for processing non-Racket assets.

If you are familiar with the JavaScript ecosystem, @racket[unlike-assets] is like
Webpack, but smaller and less opinionated. Bring elbow grease.

@include-section["fundamentals.scrbl"]
@include-section["logging.scrbl"]
@include-section["cli.scrbl"]
@include-section["inline-rfc.scrbl"]
