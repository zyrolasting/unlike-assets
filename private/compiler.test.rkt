#lang racket/base

(require
  racket/function
  racket/string
  rackunit
  [prefix-in inbox: "./inbox.rkt"]
  [prefix-in outbox: "./outbox.rkt"]
  "./assets.rkt"
  "./compiler.rkt")

;; This test only serves to verify the relationship between all compiler
;; primitives, not to kick off a complex build process.
(parameterize ([iteration-limit 10])
  (define js-req "watch-u-browz.js")
  (define expected-js "1984.js")
  (define image-req "nsfw.png")
  (define expected-image "optimal-nsfw.png")
  (define entry-req "doc")

  (define input-document
    `(html 
       (head (link ((href "https://example.com/whatever/blah/blah"))))
       (body
         (img ((src ,image-req)))
         (script ((src ,js-req))))))

  (define expected-document
    `(html 
       (head (link ((href "https://example.com/whatever/blah/blah"))))
       (body
         (img ((src ,expected-image)))
         (script ((src ,expected-js))))))

  (define expected-outbox
    (outbox:create (list (cons js-req
                               (unlike-asset expected-js null js-req))
                         (cons image-req
                               (unlike-asset expected-image null image-req))
                         (cons entry-req
                               (unlike-asset expected-document null entry-req)))))

  (define (mark-fulfilled a avail)
    (struct-copy dependent a [val #f]))

  (define (replace-nodes a avail)
    (dependent expected-document null))

  (define (load-document a avail)
    (dependent replace-nodes (list image-req js-req)))

  (define (resolve clear)
     (if (equal? clear entry-req)
         (unlike-asset load-document null clear)
         (unlike-asset (λ (asset avail)
                          (struct-copy dependent asset
                                       [val (if (string-suffix? clear ".js")
                                                expected-js
                                                expected-image)]))
                       null
                       clear)))

  (check-equal?
    (compile-unlike identity resolve entry-req)
    expected-outbox))
