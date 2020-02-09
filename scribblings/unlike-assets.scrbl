#lang scribble/manual
@require[@for-label[unlike-assets racket racket/class]]

@title{Coordinate builds consisting of unlike assets}
@author{Sage Gerard}

@defmodule[unlike-assets]

If Racket is a response to a world where real world projects use
multiple languages, then @racketmodname[unlike-assets] is a response
to a world where real world projects use a creative combination of
multiple data formats.

In the context of this collection, an @deftech{unlike asset} (or
simply "asset" when convenient) is data that you want to build into a
creative project in many ways. It's understood that any given asset is
not immediately compatible with other unlike assets. Hence the
adjective.

This collection gives you the tools to compose unlike assets in useful
ways. A website is a perfect example because content, styles and media
can be in any number of formats under any number of workflows, and
they all reference each other. @racketmodname[unlike-assets] helps you
set up and change @italic{how you work} on websites, video games art,
and multimedia compositions. An asset starts @italic{unfulfilled} and
later becomes @italic{fulfilled} by a process you decide under the
models documented herein.

@table-of-contents[]
@include-section["reactive.scrbl"]
@include-section["imperative.scrbl"]
@include-section["logging.scrbl"]
@include-section["policy.scrbl"]
