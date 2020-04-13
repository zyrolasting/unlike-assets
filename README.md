[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/unlike-assets/index.html)

Do you use Racket? Imagine this did what you meant.

```
(dynamic-rerequire "/path/to/script.js")
(dynamic-require "/path/to/script.js" 'minified)
```

That's what `unlike-assets` does.

`unlike-assets` is a bit like Webpack, except leaner and powered by
Racket. It acts as a configurable module resolver that integrates
Racket with other ecosystems. This way, you can treat any data object
like an SVG document, a CSS stylesheet, an SPIR-V shader, or a tarball
like a module with reload support.

One way this can be useful is building a CSS stylesheet according to
your preferences, so that you end up with a production-ready document
like so:

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

The implementation starts in `unlike-assets-resolver`. It provides a
configurable "module resolver" that asynchronously maps string URLs to
_up-to-date_ Racket values.  I use scare quotes because the Racket
values are not actually Racket modules, but you can tweak away
any bothersome differences.

The rest is a smorgasboard of integrations for you to sample.


## Anticipated Question: "WTF is with this filesystem. How do I require anything?!"
**Short answer**: Racket made me do it, and the docs for each package will
mention a relevant `(require ...)` lying around. Use it.

**Long answer:** Racket's module resolver thinks of _packages_ and
_collections_ as two different things. If you come from JavaScript or
Python, you can install a package and type that package's name with an
`import` or `require`.  That's NOT how it works here.

In Racket, a collection is a symbolic name for some logical group of
modules. Packages **_contribute_** to collections. So when you install
`unlike-assets-css`, it will toss a `css` module into the
`unlike-assets` collection so you can write `(require unlike-assets/css)`.

Racket (more specifically `raco`) has filesystem conventions for
supporting this kind of workflow.  This is why you see a bunch of
nested directories with the same names, and files with different names
for the same role.

## Optional Reading: Project Vision
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

If you contribute, you agree to do so under the terms of [the posted
license](./LICENSE.txt).
