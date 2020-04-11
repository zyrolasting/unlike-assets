# Changelog

This file summarizes changes to the project over time.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

Each version is in the format `major.minor`. `minor` and `major`
increments are subject to the author's interpretation of the words,
but generally mean that `major` changes have "bigger" impact than
`minor` changes. That said, _breaking changes have no relationship to
version numbers._ By policy, any breaking change is simply avoided
unless there's a good reason to introduce one. It's [too difficult to
draw a reliable relationship][jash], since some breaking changes are
meant to prepare intended functionality (e.g. fixing incorrect module
level contracts, see v1.6). The social contract is that you should not
upgrade until you have reason to do so, and if you are starting out
with this project, you should use the latest version.

## Unreleased
* Add reactive model in `reactive.rkt`
* Reorganize docs to emphasize models
* Divided source into several multi-collection packages.
* Add reactive asset definitions in `modular.rkt`.
* Expand `policy.rkt` to include helpers for `reactive.rkt`.

## [1.0] - 2019-11-21
* Add `#:strict?` keyword argument to compiler

[Unreleased]: https://github.com/zyrolasting/unlike-assets/compare/v1.0...HEAD
[1.0]: https://github.com/zyrolasting/unlike-assets/compare/

[jash]: https://gist.github.com/jashkenas/cbd2b088e20279ae2c8e
