#lang racket/base

(require racket/contract
         racket/exn
         racket/pretty
         racket/tcp
         net/url
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/dispatchers/dispatch
         unlike-assets/resolver)

(define procure-responder/c
  (-> string? (asset/c [->http-response (-> request? response?)])))

(provide
 (rename-out [asset/serveable/c asset/servable/c])
 (contract-out
  [make-dispatcher (->* () ((-> url? string?)) dispatcher/c)]
  [start-server (->* () ((-> url? string?) #:port listen-port-number?) procedure?)]
  [asset/serveable/c contract?]))

(require racket/format
         racket/function
         racket/string
         web-server/web-server
         (prefix-in lifter:
                    web-server/dispatchers/dispatch-lift))

(define asset/serveable/c
  (asset/c (->http-response (-> request? response?))))

(define (respond/text str)
  (response/output #:code 500
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (write-bytes (string->bytes/utf-8 str) o))))

(define (show-error e)
  (respond/text (exn->string e)))

(define (show-asset a)
  (respond/text
   (parameterize ([current-output-port (open-output-string)])
     (pretty-print (a))
     (get-output-string (current-output-port)))))

(define (default-url->asset-key u)
  (string-join (map path/param-path (url-path u)) "/"))

(define (make-dispatcher [url->asset-key default-url->asset-key])
  (lifter:make
   (λ (req)
     (with-handlers ([exn:fail? show-error])
       (define a (Ps (url->asset-key (request-uri req))))
       (define respond (a '->http-response (λ () (λ (req) (show-asset a)))))
       (respond req)))))

(define (start-server #:port [port 8080] [url->asset-key default-url->asset-key])
  (serve #:dispatch (make-dispatcher url->asset-key)
         #:port port))
