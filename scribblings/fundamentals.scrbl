#lang scribble/manual
@require[@for-label[unlike-assets racket racket/class]]

@title[#:tag "fundamentals"]{Fundamentals}

An @deftech{unlike asset} (or simply "asset" when convenient) is data.
Specifically, it's data that you want to use for a creative project in
many ways, but is not immediately compatible with other unlike assets.
Hence the name.

This collection gives you the tools to compose unlike assets in useful
ways. A website is a perfect example because content, styles and media
can be in any number of formats under any number of workflows, and
they all reference each other. Setting up and changing websites should
be cheap, but it isn't. @racketmodname[unlike-assets] tries to rectify
that.

An asset starts @italic{unfulfilled} and later becomes
@italic{fulfilled} by a process you decide under an imperative model
or a functional-reactive model.
