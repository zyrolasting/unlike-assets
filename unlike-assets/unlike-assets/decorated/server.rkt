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

(provide
 (all-from-out web-server/http/request-structs
               web-server/http/response-structs)
 (contract-out
  [make-dispatcher (-> resolver? dispatcher/c)]
  [start-server (->* (resolver?) (#:port listen-port-number?) procedure?)]))

(require racket/format
         racket/function
         racket/string
         web-server/web-server
         "resolve.rkt"
         (prefix-in lifter:
                    web-server/dispatchers/dispatch-lift))

(define-private serveable [make-response (-> request? response?)])

(define (respond/text code str)
  (response/output #:code code
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (write-bytes (string->bytes/utf-8 str) o))))

(define (show code val)
  (respond/text
   (parameterize ([current-output-port (open-output-string)])
     (pretty-print val)
     (get-output-string (current-output-port)))))

(define (make-dispatcher resolver)
  (lifter:make
   (λ (req)
     (with-handlers ([exn? (λ (e) (show 500 (exn->string e)))])
       (define result (resolver (url->key (request-uri req))))
       (if (serveable? result)
           ((serveable-make-response result) req)
           (show 200 result))))))

(define (start-server resolver #:port [port 8080])
  (serve #:dispatch (make-dispatcher resolver)
         #:port port))

(define (url->key u)
  (string-join (map path/param-path (url-path u)) "/"))
