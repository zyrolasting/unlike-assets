#lang scribble/manual
@(require (for-label racket racket/class unlike-assets))

@title{Command Line Interface}

Use the provided raco command to build unlike assets from the terminal.
Use @literal{raco unlike-assets:build -h} flag to see options.

@verbatim[#:indent 2]|{
$ raco unlike-assets:build my-compiler.rkt news/index.php blog/index.md shop/index.html
}|

The first argument is the Racket module that provides an instance of an @racket[unlike-compiler%] subclass
as @racket[(provide compiler)]. With the above command, the instance will be loaded using
@racket[(dynamic-require "my-compiler.rkt" 'compiler)].

Each other argument will be clarified and registered to the compiler using @method[unlike-compiler% add!].

@bold{CAUTION:} The assets you name for compilation are considered unclear and
will be arguments to @method[unlike-compiler% clarify]. If your implementation of @method[unlike-compiler% clarify]
assumes that all relative paths are relative to the same directory, then a relative
path prepared via tab completion might surprise you.

@verbatim[#:indent 2]|{
$ raco unlike-assets:build my-compiler.rkt ./project/assets/index.md # whoops
Cannot clarify ./assets/index.md
  Path not readable: /home/sage/project/assets/project/assets/index.md
  ...
}|

This source of confusion is part of the trade-off in providing consistent behavior
when representing dependencies in the abstract.

It helps to remember that as the user, you are the first unlike asset and
your dependencies are therefore subject to clarification.
