[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Imagine if Racket allowed this:

```
(dynamic-rerequire "/path/to/script.js")
(dynamic-require "/path/to/script.js" 'minified)
```

That's what Unlike Assets (UA) is about. The `unlike-assets` package
ships with a configurable module resolver for non-Racket data. You can
treat CSS, SVG, JavaScript, and other resources as if they were
software modules. In that sense, UA abstracts over creative frameworks
so that you can create your own alternatives to Polyglot, Scribble,
Frog, Universe, Pollen, etc.

This repository tracks a contained ecosystem of packages centered
around `unlike-assets`. The other packages stored here or in other
repositories include extensions with additional dependencies, which is
why they are not installed by default.

## Setup

```
git clone git@github.com:zyrolasting/unlike-assets.git
cd unlike-assets
raco pkg install ./unlike-assets
```

**This package is not (yet) on the default Racket catalog.** The
default catalog and Racket has a dependency hell problem, so there's
no good way to publish the latest version of the code without doing
some weird things. On that note, you might not be able to install some
future version of this package if you have an old version already on
your system, somewhere. For that, I'm very sorry.

If you're looking for the old editions, they are available in the
`unlike-assets-lib`, `unlike-assets-doc`, and `unlike-assets-test`
packages.
