#lang scribble/manual

@require[@for-label[racket/base unlike-assets]]

@title{Unlike Assets: Main Package Reference}
@defmodule[unlike-assets]

This is the API reference for all bindings provided by the
@tt{unlike-assets} package.

@racketmodname[unlike-assets] provides all bindings from
@racketmodname[unlike-assets/resolver],
@racketmodname[unlike-assets/resolver/file],
@racketmodname[unlike-assets/resolver/racket],
@racketmodname[unlike-assets/resolver/exn],
@racketmodname[unlike-assets/resolver/thunk],
@racketmodname[unlike-assets/config], and
@racketmodname[unlike-assets/logging].

For a friendly introduction, see @other-doc['(lib "unlike-assets/scribblings/guide/unlike-assets-guide.scrbl")].

@table-of-contents[]
@include-section{model.scrbl}
@include-section{resolver.scrbl}
@include-section{resolver-thunk.scrbl}
@include-section{resolver-file.scrbl}
@include-section{resolver-racket.scrbl}
@include-section{resolver-exn.scrbl}
@include-section{logging.scrbl}
@include-section{config.scrbl}
@include-section{u-a.scrbl}
