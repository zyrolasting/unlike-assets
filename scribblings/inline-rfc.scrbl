#lang scribble/manual

@title{Inline RFC}
@bold{This is not implemented, but would love feedback on the interface while I learn how to make it work.}

@defform[(unlike module-gen unlike-require-spec ...)
         #:contracts ([module-gen procedure?])]
Generates a @racket[require-spec] for use in @racket[require].
@racket[module-gen] runs in phase +1 to produce a
Racket module that the @racket[require-spec] will then request.

A hypothetical Racket interface for ECMAScript is as follows:

@racketblock[
(require
  (for-syntax unlike-assets        ; For `unlike`
              "./ecmascript.rkt")) ; For USER-DEFINED `ecmascript-file->racket-module`

(require
  (unlike
    ecmascript-file->racket-module
    (prefix-in client: "./client.js")
    (prefix-in server: "./server.js")))

(client:bundle
  #:allowed-module-types '(esm commonjs)
  (build-path "./bundle.js"))

(server:transpile "./transpiled.js")
]
