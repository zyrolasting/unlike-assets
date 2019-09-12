#lang racket/base

(module+ test
  (require
   rackunit
   net/url
   "./policy.rkt")

  (test-equal?
   "Paths can be resolved relative to another directory"
   (build-complete-simple-path "." (current-directory))
   (current-directory))

  (test-equal?
   "Paths are not left ambiguous"
   (build-complete-simple-path "b/.././" (current-directory))
   (current-directory))

  (test-equal?
   "Complete paths are unmodified"
   (build-complete-simple-path (current-directory))
   (current-directory))

  (test-true "Assume blank schemes are files"
             (file-url? (string->url "./blah")))

  (test-true "Don't forget the obvious"
             (file-url? (string->url "file://./blah"))))
