#lang info
(define collection "unlike-assets")
(define deps '("base" "file-watchers" "ansi-color" "graph-lib" "kinda-ferpy"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "graph-doc"))
(define scribblings '(("scribblings/unlike-assets.scrbl" (multi-page))))
(define pkg-desc "Build system for arbitrary assets")
(define version "1.1")
(define pkg-authors '(sage))
(define raco-commands
  '(("unlike-assets:build" (submod unlike-assets/cli main) "Build assets by your policy" #f)))
