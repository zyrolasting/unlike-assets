#lang scribble/manual

@title{Examples}

@section{Basic use of clarify and delegate}

@(require (for-label racket racket/class unlike-assets))

This example shows an implementation of @method[unlike-compiler% delegate] and @method[unlike-compiler% clarify]
that together build a message in terms of one set of dependencies.

@racketblock[
(require unlike-assets)

(define dependencies
  '(complexity only maddens
    people ingesting lengthy
    elusive documentation))

(define/contract (build-message clear compiler) advance/c
  (string-upcase
    (apply string (map (λ (unclear) (send compiler lookup unclear))
                   dependencies))))

(define/contract (resolve-dependency clear compiler) advance/c
  (string-ref clear 0))

(define/contract (main clear compiler) advance/c
  (define clear-names (map (λ (unclear) (send compiler clarify unclear))
                           dependencies))

  (for ([dependency clear-names])
    (send compiler add! dependency clear))

  build-message)

(define compiler (new (class* unlike-compiler% () (super-new)
                        (define/override (clarify unclear)
                          (symbol->string unclear))

                        (define/override (delegate clear)
                          (if (string=? clear "start")
                              main
                              resolve-dependency)))))

(send compiler add! "start")
(hash-ref (send compiler compile!) "start")
]

@itemlist[#:style 'ordered
@item{The compiler is informed of a new asset by an unclear name: @racket['start].}
@item{@method[unlike-compiler% clarify] maps the unclear name to @racket["start"].}
@item{@method[unlike-compiler% delegate] associates @racket["start"] to @racket[main].}
@item{@racket[main] runs, adding dependencies as a side-effect. @racket[build-message] is returned as the designated next step to fulfill @racket["start"]. It will not be called yet.}
@item{All dependencies are circulated through @method[unlike-compiler% clarify] and @method[unlike-compiler% delegate] as well, and each end up fulfilled by @racket[resolve-dependency] as the first letter of their clear names.}
@item{@racket[build-message] runs, joining the first letter of each dependency in the declared order. The output is @racket["COMPILED"]}
]

