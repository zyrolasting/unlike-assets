#lang scribble/manual

@require[@for-label[racket/base unlike-assets]]

@title{Unlike Assets: Main Package Reference}
@defmodule[unlike-assets]

This is the API reference for all bindings provided by the
@tt{unlike-assets} package.

For a friendly introduction, see @other-doc['(lib "unlike-assets/scribblings/unlike-assets-guide.scrbl")].

@table-of-contents[]
@include-section{resolver.scrbl}
@include-section{resolver-default.scrbl}
@include-section{resolver-default-extension.scrbl}
@include-section{resolver-default-file.scrbl}
@include-section{resolver-default-racket.scrbl}
@include-section{u.scrbl}
@include-section{config.scrbl}
@include-section{server.scrbl}
@include-section{distributor.scrbl}
