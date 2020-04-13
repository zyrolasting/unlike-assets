[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/unlike-assets/index.html)

Imagine that `(dynamic-require "/path/to/script.js" 'minified)`
worked. That's what `unlike-assets` does.

`unlike-assets` is a bit like Webpack, except leaner and powered by
Racket. It gives you the means to integrate Racket with other
ecosystems, so that you can treat any data object like an SVG
document, a CSS stylesheet, an SPIR-V shader, or a tarball like a
Racket module with reload support.

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
integrate Racket with other ecosystems.

The implementation starts in `unlike-assets-core`. It provides a
pluggable "module resolver" that asynchronously maps string URLs to
Racket values.  I use scare quotes because the Racket values are not
Racket modules. You can, however, configure the resolver so that
user's won't care about the difference.

`unlike-assets-conventions` tacks on usage patterns to keep the
other packages and documentation in line.

The rest is a smorgasboard for you to pick at.

Fun fact: You can build an implementation of [zyrolasting/polyglot][]
using these packages.

## Anticipated Question: "WTF is with this filesystem. How do I require anything?!"
**Short answer**: Racket made me do it, and the docs for each package will
include a relevant `(require ...)` lying around. Use it.

**Long answer:** Racket's module resolver thinks of _packages_ and
_collections_ as two different things. If you come from JavaScript or
Python, you can install a package and type that package's name with an
`import` or `require`.  That's NOT how it works here.

In Racket, a collection is a symbolic name for some logical group of
modules. Packages **_contribute_** to collections. So when you install
`unlike-assets-css`, it will toss a `css` module into the
`unlike-assets` collection so you can write `(require unlike-assets/css)`.

Racket has a filesystem convention for doing this that makes me create
a directory for each collection I want to extend. This is why you see
a bunch of nested directories with similar names.


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

Your contributions must be made under the terms of [the posted license](./LICENSE.txt).
