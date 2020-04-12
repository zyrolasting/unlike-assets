[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/unlike-assets/index.html)

Imagine that `(dynamic-require "/path/to/script.js" 'minified)`
worked. With `unlike-assets`, you won't have to.

`unlike-assets` is a bit like Webpack, except leaner and powered by
Racket. It gives you the means to integrate Racket with other
ecosystems, so that you can treat any data object like an SVG
document, a CSS stylesheet, an SPIR-V shader, or a tarball as a
restricted Racket module with reload support.

What does this mean? Well, you can build a CSS stylesheet according to
your preferences, and end up with a production-ready document like so:

```
`(link ((rel "stylesheet") (href ,(Ps& "styles.css"))))
; '(link ((rel "stylesheet") (href "../8a2b14.css")))
; "../8a2b14.css" is a minified sheet that really is one directory up
; from where this document will be in production.
```

But here's the thing: If you edit the stylesheet and move it somewhere else,
all dependent expressions remain in sync.

```
`(link ((rel "stylesheet") (href ,(Ps& "styles.css"))))
; And now it's '(link ((rel "stylesheet") (href "styles/df9012.css")))
```

`unlike-assets` helps you piece together rules between interdependent
data formats, so that chores don't interrupt your creative freedom.


## What's here?
This repository tracks a contained ecosystem of packages that
integrate Racket with _other_ ecosystems.

The `unlike-assets-{lib,test,doc}` packages constitute the core
packages of the ecosystem. `raco pkg install unlike-assets` will
install only these packages. All other packages must be installed
seperately according to user needs. If you want the leanest starting
point, just install `unlike-assets-lib`.

The extra packages contain their own tests and documentation.

## Optional Reading: Vision
My hope is to lower the barrier between Racket programmers and
incompatible technologies. By making other ecosystems act as if they
were part of Racket's module system, you can unlock massive creative
control over programming projects. I believe this is the shortest
path between Racket's ability to swap notations and the features
that cost too much to implement in pure Racket.

## Contributing
The repo is just a bucket for packages. Just toss one in package to
integrate an `unlike-assets` build system with something. Please
include tests, docs, and logging.

Your contributions must be made under the terms of [LICENSE](./LICENSE).
