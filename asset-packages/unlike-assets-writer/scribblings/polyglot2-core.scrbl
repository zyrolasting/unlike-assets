#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    unlike-assets-lib]]

@title{Polyglot2: Core}

Polyglot's second edition extends
@racketmodname[unlike-assets/reactive] with a dynamic asset resolver,
a stateful development server, and a writer.  From this, one can
create fresh development environments for web applications.
