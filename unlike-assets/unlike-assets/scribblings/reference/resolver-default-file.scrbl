#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/function
                    unlike-assets]]

@title{@tt{unlike-assets/resolver/default/file}}
@defmodule[unlike-assets/resolver/default/file]

@defproc[(file-modules [search-dirs (non-empty-listof complete-path?) (list (current-directory))])
         procedure?]{
Returns a procedure @racket[P] suitable for use in @racket[current-key->live-build].

@racket[(P key sys)] returns @racket[#f] if the key is not applicable
as a path or path string for some filesystem.

Change detection is based on @racket[file-or-directory-modify-seconds].
}
