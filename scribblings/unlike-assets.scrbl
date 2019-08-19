#lang scribble/manual
@require[@for-label[unlike-assets racket racket/class]]

@title{Coordinate builds consisting of unlike assets}
@author{Sage Gerard}

@defmodule[unlike-assets]

This collection provides tools for processing interdependent values with user-defined relationships.
If you are familiar with the JavaScript ecosystem, @racket[unlike-assets] is like Webpack, but leaner.

@table-of-contents[]

@include-section["fundamentals.scrbl"]
@include-section["rebuilds.scrbl"]
@include-section["logging.scrbl"]
@include-section["policy.scrbl"]
@include-section["cli.scrbl"]
@include-section["examples.scrbl"]
