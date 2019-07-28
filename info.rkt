#lang info
(define collection "unlike-assets")
(define deps '("base" "file-watchers"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "ansi-color"))
(define scribblings '(("scribblings/unlike-assets.scrbl" ())))
(define pkg-desc "Build system for arbitrary assets")
(define version "0.0")
(define pkg-authors '(sage))
(define raco-commands
  '(("build-unlike" unlike-assets/cli "Build assets by your policy" #f)))
