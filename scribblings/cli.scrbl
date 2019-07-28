#lang scribble/manual

@title{Command Line Interface}

Use the raco command @racket[build-unlike] to compile unlike assets from the terminal.

@verbatim[#:indent 2]|{
$ raco build-unlike my-policy.rkt news/index.php blog/index.md shop/index.html
}|

The first argument is the Racket module that provides @racket[clarify] and @racket[resolve] as
procedures. Both are loaded using @racket[dynamic-require]. Each argument thereafter will
appear in a call to @racket[compile-all-unlike] or @racket[compile-unlike], depending on the
number of entries provided.

Command line flags control @secref{params}. Use @literal{raco compile-unlike -h} flag for reference.

