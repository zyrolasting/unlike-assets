#lang racket/base

(require racket/contract
         racket/exn
         racket/format
         racket/function
         racket/pretty
         racket/string
         racket/tcp
         net/url
         unlike-assets
         web-server/dispatchers/dispatch
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/web-server
         (prefix-in lifter:
                    web-server/dispatchers/dispatch-lift))


(struct serveable (make-response))
(define serveable/c (struct/c serveable (or/c (-> request? response?) response?)))
(define server-seat/c (seat/c serveable?))

(provide
 (all-from-out web-server/http/request-structs
               web-server/http/response-structs)
 (struct-out serveable)
 (contract-out
  [serveable/c contract?]
  [server-seat/c contract?]
  [make-dispatcher (-> server-seat/c dispatcher/c)]
  [start-server (->* (server-seat/c) (#:port listen-port-number? (-> url? any/c)) procedure?)]))


(define (respond/text code str)
  (response/output #:code code
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (write-bytes (string->bytes/utf-8 str) o))))


(define (show code val)
  (respond/text code
   (parameterize ([current-output-port (open-output-string)])
     (pretty-print val)
     (get-output-string (current-output-port)))))


(define (make-dispatcher seat)
  (lifter:make
   (λ (req)
     (with-handlers ([exn? (λ (e) (respond/text 500 (exn->string e)))])
       (define maybe-resp ((seat req)))
       (show 200
             (if (response? maybe-resp)
                 maybe-resp
                 (maybe-resp req)))))))


(define (start-server seat #:port [port 8080])
  (serve #:dispatch (make-dispatcher seat)
         #:port port))
