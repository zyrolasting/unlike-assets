#lang scribble/manual
@require[@for-label[unlike-assets racket racket/class]]

@title{Coordinate builds consisting of unlike assets}
@author{Sage Gerard}

@defmodule[unlike-assets]

This collection provides tools for processing assets. In the context
of this collection, an @deftech{asset} is a creative artifact that you
can build. It can be a web page, a software package, or a video game
level.

Real world projects tend to be a collection of @italic{unlike} assets.
If Racket is a response to a world where real world projects use
multiple languages, then @racketmodname[unlike-assets] is a response
to a world where real world projects use a creative combination of
multiple data formats.

If you are familiar with the JavaScript ecosystem,
@racket[unlike-assets] is like Webpack, but leaner.

@table-of-contents[]

@include-section["fundamentals.scrbl"]
@include-section["imperative.scrbl"]
@include-section["reactive.scrbl"]
@include-section["rebuilds.scrbl"]
@include-section["logging.scrbl"]
@include-section["policy.scrbl"]
@include-section["cli.scrbl"]
@include-section["examples.scrbl"]
