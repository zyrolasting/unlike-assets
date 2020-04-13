#lang racket/base

(provide (all-defined-out))

(require racket/contract
         project-paths
         web-server/http/request-structs
         web-server/http/response-structs
         unlike-assets/core)

(define asset/writable/c
  (asset/c [write-bytes (-> output-port? any)]))
(define asset/writeable/c asset/writable/c) ; Sometimes code can save time in unexpected ways.

(define asset/file-sourced/c
  (asset/c [input-file-path (and/c complete-path? file-exists?)]))
(define asset/file-destined/c
  (asset/c [output-file-path complete-path?]))
(define asset/file-to-file/c
  (and/c asset/file-sourced/c
         asset/file-destined/c))

(define asset/serveable/c
  (asset/c (->http-response (-> request? response?))))
(define asset/servable/c asset/serveable/c)

; Represents an website resource that starts as a file, ends as
; another file, and is writable in both as a file and as a web server
; response.
(define asset/webdev/local/c
  (and/c asset/file-to-file/c
         asset/writable/c
         asset/serveable/c))
