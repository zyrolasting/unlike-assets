#lang racket

(provide (all-defined-out))

(require tesurell
         racket/random
         "../../live-asset.rkt"
         "../../paths.rkt"
         "../../system/main.rkt"
         "../../feeds.rkt"
         "../model.rkt"
         "../highlight-code.rkt"
         "../metadata.rkt"
         "html-elements.rkt")

; TODO: Include OpenXML elements and means to switch between them.
; For now, it's HTML only.

; I borrow some of Scribble's identifiers
; so I don't always have to remember two sets of names.
(define bold h:strong)
(define italic h:em)
(define inline h:span)
(define tt h:code)
(define item h:li)

(define (recite #:src [src #f]  . strs)
  (apply h:blockquote
         (if src
             (append strs
                     (list (h:footer src)))
             strs)))

(define (cq . strs)
  (~a "“" (apply string-append strs) "”"))

(define (todo . strs)
  (apply h:span #:class "todo" strs))

(define (make-section-proc el)
  (λ (#:tag [t #f] . children)
    (apply el #:id (or t (make-slug (apply ~a children)))
           children)))

(define section (make-section-proc h:h2))
(define subsection (make-section-proc h:h3))
(define subsubsection (make-section-proc h:h4))

(define (hl href . children)
  (apply h:a #:href (if (or (string? href) (procedure? href))
                        href
                        (λ () (hash-ref (current-links) href)))
         children))

(define (pl key . children)
  (require-live-asset key)
  (apply h:a #:href key
         children))

(define (itemlist #:style [type 'unordered] . children)
  (apply (if (eq? type 'ordered) h:ol h:ul)
         children))

; A bibliography system just needs a hash and a way to delay looking
; up links until the hash is done.
(define current-links (make-parameter #hash()))

(define (make-bibliography . items)
  (define (label? i) (eq? 0 (modulo i 2)))
  (define L (length items))
  (when (> (modulo L 2) 0)
    (raise-argument-error 'bib
                          "A list of even length, with elements like (non-string string non-string string ...)"
                          items))
  (for/fold ([lookup #hash()])
            ([i (in-range (sub1 L))])
    (if (label? i)
        (hash-set lookup (list-ref items i) (list-ref items (add1 i)))
        lookup)))

(define (bib . items)
  (current-links (apply make-bibliography items)))

(define (my-email)
  (h:span #:style "unicode-bidi:bidi-override; direction: rtl;"
          "moc.drar"
          (h:span #:style "display: none"
                  (apply string
                         (random-sample "abcdefghijklmnopqrstuvwxyz"
                                        (random 2 6)
                                        #:replacement? #t)))
          "egegas@egas"))

(define (hero title)
  (h:section #:class "hero" (h:h1 title)))

(define (updated-date dt)
  (h:span #:class "header-updated-date"
         "Last updated: "
         (h:time #:datetime dt dt)))

(define (article-header last-updated-date)
  (h:header (h:div #:class "header-meta"
                   (updated-date last-updated-date)
                   (h:a #:href "index.html" "home"))))

(define (codeblock lang . lines)
  (car (highlight-code
        lang
        (string-trim (string-join lines "")))))

(define (racket-example . lines)
  (h:div (apply codeblock "racket" lines)
         (h:output (apply embed (string->symbol (symbol->string (gensym))) 'out lines))))

(define (feed . comma-separated-topics)
  (define key (make-feed-key (apply ~a comma-separated-topics)))
  (define posts (rss-feed-posts (require-live-asset key)))
  (apply h:div #:class "feed"
         (h:a #:class "rss-link" #:href (& key) "rss")
         (map post-listing-link posts)))

(define (post-listing-link p)
  (h:a #:class "post-listing-link" #:href (live-asset-alias p)
       (h:span #:class "post-listing-title" (post-title p))
       (h:span #:class "post-listing-date" (post-published-date p))))
