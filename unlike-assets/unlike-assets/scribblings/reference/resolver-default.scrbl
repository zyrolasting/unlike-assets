#lang scribble/manual

@require[@for-label[racket/base unlike-assets/resolver/default]]

@title{@tt{unlike-assets/resolver/default}}
@defmodule[unlike-assets/resolver/default]

The default resolver implementation produces @racket[hasheq] values
to represent supported data formats.

@racketmodname[unlike-assets/resolver/default] reprovides all bindings
from @racketmodname[unlike-assets/resolver/default/racket],
@racketmodname[unlike-assets/resolver/default/file], and
@racketmodname[unlike-assets/resolver/default/extension].
