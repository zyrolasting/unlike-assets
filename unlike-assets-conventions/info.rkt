#lang info

(define pkg-desc "Conventions for all optional unlike-assets packages")
(define pkg-authors '(sage))

(define collection 'multi)

(define deps '("base" "unlike-assets-core" "project-paths" "web-server-lib" "reprovide-lang-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
