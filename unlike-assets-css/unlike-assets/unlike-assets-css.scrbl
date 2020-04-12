#lang scribble/manual

@title{Unlike Asset: Cascading Style Sheets (CSS)}

@require[@for-label[racket/base]]
@defmodule[unlike-assets/css]

This module integrates CSS with an
@racketmodname[unlike-assets/reactive] build system.  It provides a
pure-Racket CSS preprocessor, a hybrid language of CSS and Racket, and
minified output.

@section{Racket-CSS Hybrid Language}
When used as a @litchar{#lang}, @racketmodname[unlike-assets/css]
reads a section of Racket code followed by a section of CSS
Expressions as per @racketmodname[css-expr]. All style declarations
are accumulated, then provided as a minified string.

The Racket section has all of the bindings from
@racketmodname[racket/base], @racketmodname[unlike-assets/css], and
@racketmodname[css-expr] available for use.

@racketmod[unlike-assets/css

(code:comment "You can write racket/base code normally")
(require racket/format)

(code:comment "You can declare variables for later interpolation.")
(define color '|#888|)

(code:comment "You can use macros in the Racket section to add style declarations")
(code:comment "that would be painful to write by hand.")
(add-css-expr! (font-face "code" "fonts/sourcecodepro-bold-webfont" 'normal 'bold))

(code:comment "Add three dashes to tell the reader to start")
(code:comment "reading CSS expressions.")
---

[* #:box-sizing border-box]
[h1 #:text-transform uppercase]
[.my-widget #:color ,color]
]

@section{API Reference}
