#lang scribble/manual
@require[@for-label[unlike-assets racket racket/class]]

@title{Unlike Assets: Build Video Games, Websites, and Other Creative Projects}
@author{Sage Gerard}

@defmodule[unlike-assets]

Unlike Assets (UA) builds websites, video games, music compositions,
and other complicated creative projects. It's like
@hyperlink["https://webpack.js.org/"]{Webpack}, but better.

UA works by naming arbitrary Racket values and tracking them in a
dependency graph. This enables two powerful changes in your work:

@itemlist[
@item{It keep resources in sync while you work, so if you (for
example) edit a stylesheet, all dependendent web pages will
automatically reference the production-ready version of the styles.}
@item{You can compose data in interesting ways.
You can make an image depend on music to produce a live visualization.
You can make a 3D model depend on a URL such that changing the URL loads
a texture from that location.}]

@table-of-contents[]
@include-section["reactive.scrbl"]
@include-section["imperative.scrbl"]
@include-section["logging.scrbl"]
@include-section["policy.scrbl"]
