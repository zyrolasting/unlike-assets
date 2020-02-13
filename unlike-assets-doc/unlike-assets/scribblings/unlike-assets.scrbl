#lang scribble/manual
@require[@for-label[unlike-assets racket racket/class]]

@title{Coordinate builds consisting of unlike assets}
@author{Sage Gerard}

@defmodule[unlike-assets]

If Racket is a response to real world projects using multiple
languages, then this is a response to real
world projects using multiple data formats.

I refer to this project as Unlike Assets, UA, or by a link like
@racketmodname[unlike-assets]. But as a user, an @deftech{unlike asset} (or
simply "asset") is data that you want to build into a creative
project. Any given asset might not be immediately compatible with
other assets. Hence "unlike".

@margin-note{For an example of a project that uses Unlike Assets, see
@other-doc['(lib
"polyglot/scribblings/reference/polyglot-reference.scrbl")]} This
collection gives you the tools to compose unlike assets in useful
ways. A website, for example, combines content, presentation and
behavior using any number of formats under any number of
workflows. You could use UA to help you set up and change @italic{how
you work} on websites, video games art, and multimedia
compositions. An asset starts @italic{unfulfilled} and later becomes
@italic{fulfilled} by a process you decide under the models documented
herein.

@table-of-contents[]
@include-section["reactive.scrbl"]
@include-section["imperative.scrbl"]
@include-section["logging.scrbl"]
@include-section["policy.scrbl"]
