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


(provide
 (all-from-out web-server/http/request-structs
               web-server/http/response-structs)
 (contract-out
  [make-dispatcher-with-seat
   (->* ((seat/c any/c)) (#:on-error (-> request? exn? response?)
                          #:on-other (-> request? any/c response?))
        dispatcher/c)]
  [serve-seat (->* ((seat/c any/c))
                   (#:port listen-port-number?
                    #:on-error (-> request? exn? response?)
                    #:on-other (-> request? any/c response?))
                   procedure?)]))


(define (respond/text code str)
  (response/output #:code code
                   #:mime-type #"text/plain; charset=utf-8"
                   (λ (o) (write-bytes (string->bytes/utf-8 str) o))))


(define (show code val)
  (respond/text code
   (parameterize ([current-output-port (open-output-string)])
     (pretty-print val)
     (get-output-string (current-output-port)))))

(define (default-on-error r e)
  (respond/text 500 (exn->string e)))

(define (default-on-other r v)
  (show 200 v))

(define (make-dispatcher-with-seat seat
                                   #:on-error [on-error default-on-error]
                                   #:on-other [on-other default-on-other])
  (lifter:make
   (λ (req)
     (with-handlers ([exn? (λ (e) (on-error req e))])
       (define maybe-resp ((seat req)))
       (if (response? maybe-resp)
           maybe-resp
           (on-other req maybe-resp))))))

(define (serve-seat seat
                    #:port [port 8080]
                    #:on-error [on-error default-on-error]
                    #:on-other [on-other default-on-other])
  (serve #:dispatch (make-dispatcher-with-seat seat)
         #:port port))
