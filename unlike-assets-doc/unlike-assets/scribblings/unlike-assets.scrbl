#lang scribble/manual
@require[@for-label[unlike-assets racket racket/class]]

@title{Unlike Assets: Build Video Games, Websites, and Other Creative Projects}
@author{Sage Gerard}

@defmodule[unlike-assets]

Unlike Assets (UA) builds websites, video games, music compositions,
and other complicated creative projects. It is functionally similar to
@hyperlink["https://webpack.js.org/"]{Webpack}, but is much leaner and
more flexible.

UA works by naming arbitrary Racket values and tracking them in a
dependency graph. It keep resources in sync while you work, so
if you (for example) edit a stylesheet, all dependendent web pages
will automatically reference the production-ready version of that sheet.

@table-of-contents[]
@include-section["reactive.scrbl"]
@include-section["imperative.scrbl"]
@include-section["logging.scrbl"]
@include-section["policy.scrbl"]
