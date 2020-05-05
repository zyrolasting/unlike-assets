#lang racket/base

(provide u/a-cli
         u/a-cli/distribute
         u/a-cli/serve)
(require racket/cmdline
         racket/logging
         racket/tcp
         raco/command-name
         unlike-assets/resolver
         "logging.rkt"
         "server.rkt"
         "distributor.rkt")

(define (u/a-cli/serve resolver)
  (define port 8080)
  (command-line
   #:program "serve"
   #:once-each
   [("-p" "--port") user-port
    "If -s is set, sets the port on which to listen for connections."
    (begin
      (define n (string->number user-port))
      (if (listen-port-number? n)
          (set! port n)
          (error 'unlike-assets "Invalid port: ~a" n)))]
   #:args ()
   (printf "Listening on port ~a~n" port)
   (displayln "Stop the server with ^C")
   (define stop (start-server resolver #:port port))
   (dynamic-wind void
                 (λ () (sync/enable-break never-evt))
                 stop)))

(define (u/a-cli/distribute resolver)
  (command-line
   #:program "distribute"
   #:once-each
   [("-n" "--no-dry-run") "Disable dry run." (dry-run-enabled #f)]
   #:args keys
   (for ([k (in-list keys)])
     (procure k)
     (log-unlike-assets-info "procured: ~a" k))
   (sync-filesystem-to-resolver-cache! resolver)))

(define (default-run-action action)
  ((case action
    [("distribute") u/a-cli/distribute]
    [("serve") u/a-cli/serve]
    [else (λ (r)
            (eprintf "Unknown action: ~a~n" action)
            (exit 1))])
   (current-resolver)))

(define (u/a-cli [run-action default-run-action])
  (define level 'info)
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("-v" "--verbose") "Show debug level logs" (set! level 'debug)]
   #:args (action . others)
   (with-logging-to-port
     (current-output-port)
     (λ ()
       (void (with-handlers ([exn:break? (λ _ (displayln "User break"))])
               (parameterize ([current-command-line-arguments (list->vector others)])
                 (run-action action)))))
     #:logger unlike-assets-logger
     level
     'unlike-assets)))
