#lang info
(define collection "unlike-assets")
(define deps '("base" "file-watchers" "ansi-color" "graph-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/unlike-assets.scrbl" (multi-page))))
(define pkg-desc "Build system for arbitrary assets")
(define version "1.0")
(define pkg-authors '(sage))
(define raco-commands
  '(("unlike-assets:build" (submod unlike-assets/cli main) "Build assets by your policy" #f)))
