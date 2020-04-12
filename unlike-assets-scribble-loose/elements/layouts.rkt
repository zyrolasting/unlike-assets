#lang racket/base

(provide (all-defined-out))
(require "html-elements.rkt"
         "abstract-elements.rkt"
         "../metadata.rkt")

(define (page body)
  (h:html (h:head (h:meta #:charset "utf-8")
                  (h:title (format "~~slg: ~a" (title)))
                  (h:style "html{visibility:hidden;opacity:0;") ; fixes fouc
                  (h:meta #:name "viewport" #:content "width=device-width, initial-scale=1")
                  (h:meta #:name "description" #:content (summary))
                  (h:link #:rel "stylesheet" #:type "text/css" #:href (& "styles/main.rkt")))
          (children-only h:body body)))

(define (article body)
  (page (list (hero (title))
              (h:main (children-only h:article
                                     (cons (article-header (last-updated-date))
                                           body))))))
