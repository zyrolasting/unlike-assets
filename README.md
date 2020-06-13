[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Imagine if Racket allowed this:

```
(dynamic-rerequire "/path/to/script.js")
(dynamic-require "/path/to/script.js" 'minified)
```

Unlike Assets (UA) helps you write module resolvers for data outside
of Racket. You can treat CSS, SVG, JavaScript, and other resources as
if they were software modules. In that sense, UA abstracts over
creative frameworks so that you can create your own alternatives to
Polyglot, Scribble, Frog, Universe, Pollen, etc.

This repository tracks a contained ecosystem of packages centered
around `unlike-assets`. The other packages stored here or in other
repositories include extensions with additional dependencies, which is
why they are not installed by default.
