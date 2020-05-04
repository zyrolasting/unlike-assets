#lang racket/base

(require racket/contract
         racket/exn
         racket/format
         racket/function
         racket/pretty
         racket/string
         racket/tcp
         net/url
         unlike-assets/resolver
         web-server/dispatchers/dispatch
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/web-server
         "resolver/extension.rkt"
         (prefix-in lifter:
                    web-server/dispatchers/dispatch-lift))

(provide
 (all-from-out web-server/http/request-structs
               web-server/http/response-structs)
 (hash-partition-out serveable
                     [make-response (or/c response? (-> request? response?))])
 (contract-out
  [make-dispatcher (-> resolver? (-> url? any/c) dispatcher/c)]
  [start-server (->* (resolver?) (#:port listen-port-number? (-> url? any/c)) procedure?)]))

(hash-partition serveable (make-response))

(define (respond/text code str)
  (response/output #:code code
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (write-bytes (string->bytes/utf-8 str) o))))

(define (show code val)
  (respond/text code
   (parameterize ([current-output-port (open-output-string)])
     (pretty-print val)
     (get-output-string (current-output-port)))))

(define (make-dispatcher resolver url->key)
  (lifter:make
   (λ (req)
     (with-handlers ([exn? (λ (e) (respond/text 500 (exn->string e)))])
       (define result ((resolver (url->key (request-uri req)))))
       (if (serveable? result)
           (let ([maybe-resp (serveable-make-response result)])
             (if (response? maybe-resp)
                 maybe-resp
                 (maybe-resp req)))
           (show 200 result))))))

(define (start-server resolver [url->key default-url->key] #:port [port 8080])
  (serve #:dispatch (make-dispatcher resolver url->key)
         #:port port))

(define (default-url->key u)
  (string-join (map path/param-path (url-path u)) "/"))
