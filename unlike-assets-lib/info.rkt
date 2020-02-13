#lang info
(define collection 'multi)
(define deps '("base" "file-watchers" "ansi-color" "graph-lib" "kinda-ferpy"))
(define pkg-desc "Implementation for unlike-assets")
(define version "1.1")
(define pkg-authors '(sage))
(define raco-commands
  '(("unlike-assets:build" (submod unlike-assets/cli main) "Build assets by your policy" #f)))
