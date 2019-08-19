#lang scribble/base
#
@require[scriblib/footnote]

@define-footnote[trinity-note mark-note]

Like all aspirational projects, Webpack made tradeoffs.
For one, Webpack users make input languages behave in non-standard ways
for common use cases.@trinity-note{This isn't
surprising considering that most client-side web development is locked to
HTML, CSS and JavaScript, which cannot evolve as quickly as our aspirations for them.}
A common pipeline for CSS files, for example, allows the CSS @literal{@"@"import}
declaration to expand one stylesheet within another. The
standard behavior is to kick off a new request for a stylesheet serially. This and other such
re-interpretations of input language behavior creates a world where one module
declared for one team may behave entirely diffently between Webpack
configurations. Worse, a programmer may develop the habit of assuming that the
non-standard behavior is normal and acceptable so long as Webpack is
well-adopted. To continue the last example, use of @literal{@"@"import} is
not encouraged for CSS delivered to a client, but results in well-performing
code so long as you use Webpack.

Webpack's configuration-centric interface also obliges users
to maintain correct behavior outside of a black box. All assets
and their transformations are covered by a monolithic configuration
that is difficult to change without cascading side-effects inside the
black box. The more mature the Webpack configuration, the more fragile
the compiler.

@literal{unlike-assets} gives Racket Webpack's powers with different
trade-offs. Assets are processed entirely by user code, which increases
freedom when dealing with corner-cases at the cost of a higher workload for
setting up a build. There are no plugins or loaders. The entire journey
of an asset from disk or over the network is managed by your explicit
instructions. That way if something goes wrong, you can fix it where
the problem occurs without needing to know how the compiler works.

@mark-note[]
