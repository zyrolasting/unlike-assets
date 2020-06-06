#lang scribble/manual

@require[@for-label[racket/base unlike-assets]]

@title{Unlike Assets: Main Package Reference}
@defmodule[unlike-assets]

This is the API reference for all bindings provided by the
@tt{unlike-assets} package.

For a friendly introduction, see @other-doc['(lib "unlike-assets/scribblings/unlike-assets-guide.scrbl")].

@table-of-contents[]
@include-section{resolver.scrbl}
@include-section{resolver-extension.scrbl}
@include-section{config.scrbl}
@include-section{u-a.scrbl}
@include-section{resolver-file.scrbl}
@include-section{resolver-racket.scrbl}
@include-section{logging.scrbl}
