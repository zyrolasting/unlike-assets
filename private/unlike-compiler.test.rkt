#lang racket/base

(module+ test
  (require
   racket/class
   racket/function
   racket/list
   racket/string
   rackunit
   "./assets.rkt"
   "./unlike-compiler.rkt")

  (test-case
    "(delegate) example"
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
      (make-immutable-hash
       `((,js-req . ,expected-js)
         (,image-req . ,expected-image)
         (,entry-req . ,expected-document))))

    (define (mark-fulfilled . _) #f)
    (define (replace-nodes  . _) expected-document)
    (define (load-document clear compiler)
      (send compiler add! image-req)
      (send compiler add! js-req)
      replace-nodes)

    (define compiler
      (new (class* unlike-compiler% ()
             (super-new)
             (define (delegate clear)
               (if (equal? clear entry-req)
                   load-document
                   (λ _ (if (string-suffix? clear ".js")
                            expected-js
                            expected-image))))
             (override delegate))))

    (send compiler add! (send compiler clarify entry-req))
    (check-equal? (send compiler compile!) expected-outbox))

  (test-case
    "Live build"

    ;; The checks lean heavily on counting what clear names appear in each stage
    ;; ---
    (define logs (make-hasheq))

    (define (record! proc clear)
      (hash-set! logs proc (cons clear (hash-ref logs proc null))))

    (define (appeared-count proc clear)
      (count (λ (other) (equal? clear other))
             (hash-ref logs proc)))

    (define (check-counts proc expected-counts)
      (for ([expected expected-counts])
        (check-equal? (appeared-count proc (car expected))
                      (cdr expected))))

    ;; Throwaway advance/c procedures
    ;; ---
    (define (minify clear compiler) (record! minify clear) 'done)
    (define (finish-html clear compiler) (record! finish-html clear) 'done)
    (define (replace-nodes clear compiler) (record! replace-nodes clear) finish-html)
    (define (start-html clear compiler) (record! start-html clear) replace-nodes)

    (define compiler
      (new (class* unlike-compiler% ()
             (super-new)
             (define/override (delegate clear)
               (if (string-suffix? clear ".html")
                   start-html
                   minify)))))

    (define (check-output output)
      (check-true (andmap (λ (s) (equal? s 'done))
                          (hash-values output))))

    ;; This blocks change from propagating
    (define (immune from to history) (car history))

    (send compiler add! "index.html")
    (send compiler add! "secondary.html" "index.html"     immune)
    (send compiler add! "tertiary.html"  "secondary.html" immune)
    (send compiler add! "styles.css"     "index.html"     (λ _ replace-nodes))
    (send compiler add! "styles.css"     "secondary.html" (λ _ replace-nodes))
    (send compiler add! "styles.css"     "tertiary.html"  (λ _ replace-nodes))
    (send compiler add! "behavior.js"    "index.html"     (λ _ replace-nodes))
    (send compiler add! "behavior.js"    "secondary.html" (λ _ replace-nodes))
    (check-output (send compiler compile!))

    (check-counts minify
                  '(("index.html" . 0)
                    ("secondary.html" . 0)
                    ("tertiary.html" . 0)
                    ("styles.css" . 1)
                    ("behavior.js" . 1)))

    (for ([proc (list start-html replace-nodes finish-html)])
      (check-counts proc
                    '(("index.html" . 1)
                      ("secondary.html" . 1)
                      ("tertiary.html" . 1)
                      ("styles.css" . 0)
                      ("behavior.js" . 0))))

    (check-output (send compiler compile! #:changed '("tertiary.html")))
    (for ([proc (list start-html replace-nodes finish-html)])
      (check-counts proc
                    '(("index.html" . 1)      ; -+
                      ("secondary.html" . 1)  ;  +-- This shows a regression such that dependents were immune to the change.
                      ("tertiary.html" . 2)   ; -+
                      ("styles.css" . 0)
                      ("behavior.js" . 0))))

    ; Changing JS rebuilds the JS, and only regresses dependent HTML partially
    (check-output (send compiler compile! #:changed '("behavior.js")))
    (check-counts minify
                  '(("index.html" . 0)
                    ("secondary.html" . 0)
                    ("tertiary.html" . 0)
                    ("styles.css" . 1)
                    ("behavior.js" . 2)))
    (check-counts start-html
                  '(("index.html" . 1)
                    ("secondary.html" . 1)
                    ("tertiary.html" . 2)
                    ("styles.css" . 0)
                    ("behavior.js" . 0)))
    (check-counts replace-nodes
                  '(("index.html" . 2)
                    ("secondary.html" . 2)
                    ("tertiary.html" . 2)
                    ("styles.css" . 0)
                    ("behavior.js" . 0)))

    ; Changing CSS rebuilds the CSS, and only regresses dependent HTML partially
    (check-output (send compiler compile! #:changed '("styles.css")))
    (check-counts minify
                  '(("index.html" . 0)
                    ("secondary.html" . 0)
                    ("tertiary.html" . 0)
                    ("styles.css" . 2)
                    ("behavior.js" . 2)))
    (check-counts start-html
                  '(("index.html" . 1)
                    ("secondary.html" . 1)
                    ("tertiary.html" . 2)
                    ("styles.css" . 0)
                    ("behavior.js" . 0)))
    (check-counts replace-nodes
                  '(("index.html" . 3)
                    ("secondary.html" . 3)
                    ("tertiary.html" . 3)
                    ("styles.css" . 0)
                    ("behavior.js" . 0)))

    ; Try bulk changes and a duplicate for good measure. Make sure the HTML regresses only once.
    (check-output (send compiler compile! #:changed '("styles.css" "behavior.js" "behavior.js")))
    (check-counts minify
                  '(("index.html" . 0)
                    ("secondary.html" . 0)
                    ("tertiary.html" . 0)
                    ("styles.css" . 3)
                    ("behavior.js" . 3)))
    (check-counts start-html
                  '(("index.html" . 1)
                    ("secondary.html" . 1)
                    ("tertiary.html" . 2)
                    ("styles.css" . 0)
                    ("behavior.js" . 0)))
    (check-counts replace-nodes
                  '(("index.html" . 4)
                    ("secondary.html" . 4)
                    ("tertiary.html" . 4)
                    ("styles.css" . 0)
                    ("behavior.js" . 0)))

    (check-output (send compiler compile! #:removed '("secondary.html")))
    (check-counts start-html
                  '(("index.html" . 2)
                    ("secondary.html" . 1)
                    ("tertiary.html" . 2)
                    ("styles.css" . 0)
                    ("behavior.js" . 0)))
    (check-counts replace-nodes
                  '(("index.html" . 5)
                    ("secondary.html" . 4)
                    ("tertiary.html" . 4)
                    ("styles.css" . 0)
                    ("behavior.js" . 0))))

  (test-case "Use of non-strict"
    (define counts 0)
    (define compiler
      (new (class unlike-compiler% (super-new)
             (define/override (delegate clear)
               (λ _ (set! counts (add1 counts)))))))

    (send compiler add! "a")
    (send compiler add! "b" "a")
    (send compiler add! "c" "a")
    (send compiler compile!)
    (define ncalls counts)
    (send compiler compile! #:changed '("z") #:strict? #f)
    (send compiler compile! #:removed '("u") #:strict? #f)
    (check-equal? ncalls counts)))

