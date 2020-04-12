#lang racket/base

(provide (all-defined-out))
(require racket/function
         racket/format
         racket/path
         "system.rkt"
         "paths.rkt"
         "shared.rkt")

; This is the canonical way to find the href value of an asset loaded by key.
; e.g. `(a ((href ,(find-href $src $dest "styles.css"))) "See stylesheet")
;
; $src is the complete path of the asset file requesting the href
; value.  $dest is the complete path of the output file
; requesting the href value.  You need $src to infer if an asset is
; asking for itself, and you need $dest to compute a relative path
; from the requesting asset output file to the requested asset's
; output file.
(define (find-href dependent key)
  (path->string
   (build-path 'same
               (if (equal? (make-asset-path key) (dependent 'src))
                   (file-name-from-path (dependent 'dest))
                   (let ([dependency (polyglot-require key)])
                     (find-relative-path (path-only (dependency 'dest))
                                         (dependent 'dest)))))))

(define current-input-document-path (make-parameter #f))
(define current-output-document-path (make-parameter #f))
(define current-working-document
  (make-derived-parameter current-input-document-path
                          guard-document-path-switch!
                          values))

(define (& key)
  (thunk (find-href (current-input-document-path)
                    (current-output-document-path)
                    key)))
