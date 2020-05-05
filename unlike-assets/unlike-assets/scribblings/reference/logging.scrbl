#lang scribble/manual

@require[@for-label[racket/base unlike-assets]]

@title{Logging}
@defmodule[unlike-assets/logging]

This module provides all bindings produced by @racket[(define-logger
unlike-assets)]. This logger should be used internally by the library,
and by any package authors that ship resolver extensions.
