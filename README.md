[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/unlike-assets/index.html)

Imagine if Racket allowed this:

```
(dynamic-rerequire "/path/to/script.js")
(dynamic-require "/path/to/script.js" 'minified)
```

That's what `unlike-assets` does. It ships with a configurable module
resolver for non-Racket ecosystems. Under this light, an SVG document,
a CSS stylesheet, an SPIR-V shader, or a tarball are importable
objects that are never out of date.

Let's say you reference a stylesheet using one of the _procure_
functions provided by this project:

```
`(link ((rel "stylesheet") (href ,(Ps& "styles.css"))))
```

Given your configuration, this will produce a production-ready
variant of the element.

```
'(link ((rel "stylesheet") (href "../8a2b14.css")))
```

In this context, `../8a2b14.css` is a minified sheet that _really is_
one directory up from where this document will be in production. That
level of correctness is derived from your configuration.

Assume you edit the stylesheet and move it somewhere else while you work.
All you have to do is evaluate the dependent expression again.

```
`(link ((rel "stylesheet") (href ,(Ps& "styles.css"))))
; And now it's '(link ((rel "stylesheet") (href "styles/df9012.css")))
```

## What's Here?
This repository tracks a contained ecosystem of packages. It all starts in
`unlike-assets-resolver`, which provides a configurable module resolver
that asynchronously maps string URLs to up-to-date hashes.

The other packages play different roles. Most of them define a kind of
asset that the resolver can use. Others offer recipes that ease use
of lower-level packages. If you don't want to think about it too much,
you'll just want the `unlike-assets` package, which bundles useable
defaults with a `#lang`.

Once the defaults no longer suffice, you will need to install
the packages relevant to you.

## WTF is with this filesystem?
**Short answer**: Racket made me do it.

**Long answer:** Racket's module resolver thinks of _packages_ and
_collections_ as two different things. If you come from JavaScript or
Python, you can install a package and type that package's name with an
`import` or `require`. _That's not how it works here._

In Racket, a collection is a logical grouping of modules. Packages
**_contribute_** to collections. So when you install
`unlike-assets-css`, it will toss a `css` module into the
`unlike-assets` collection so you can write `(require
unlike-assets/css)`.

Racket and it's package manager `raco` have file conventions for this
reason and others I don't mention. This is why you see a bunch of
nested directories with the same names, and files with different names
for the same role. I'll probably end up writing a tutorial for that at
some point.


## Contributing
The repo is just a bucket for packages. Just toss one in package to
integrate an `unlike-assets` build system with something. Please
include tests, docs, and logging.

If you contribute, you agree to do so under the terms of [the posted
license](./LICENSE.txt).
