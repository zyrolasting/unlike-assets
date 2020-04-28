#lang racket/base

(provide u/a-cli)
(require racket/cmdline
         racket/logging
         racket/tcp
         raco/command-name
         unlike-assets/resolver
         "logging.rkt"
         "server.rkt"
         "distributor.rkt")

(define (u/a-cli)
  (define port 8080)
  (define level 'info)

  (define (run-server/wait)
    (printf "Listening on port ~a~n" port)
    (displayln "Stop the server with ^C")
    (define stop (start-server (current-resolver) #:port port))
    (dynamic-wind void
                  (λ () (sync/enable-break never-evt))
                  stop))

  (define (distribute)
    (sync-filesystem-to-resolver-cache!))

  (define action
    (λ () (displayln "No action specified. Run with -h for help.")))

  (command-line
   #:program (short-program+command-name)
   #:usage-help
   "Compile <asset-refs> according to <policy-module>."
   "Run `raco doc unlike-assets` for documentation."
   #:once-each
   [("-p" "--port") user-port
    "If -s is set, sets the port on which to listen for connections."
    (begin
      (define n (string->number user-port))
      (if (listen-port-number? n)
          (set! port n)
          (error 'unlike-assets "Invalid port: ~a" n)))]
   [("-n" "--no-dry-run")
    "Disable dry run."
    (dry-run-enabled #f)]
   [("-v" "--verbose")
    "Show debug level logs"
    (set! level 'debug)]
   #:once-any
   [("-s" "--server")
    "Start server"
    (set! action run-server/wait)]
   [("-d" "--distribute")
    "Distribute files"
    (set! action distribute)]
    #:args keys
    (with-logging-to-port
      (current-output-port)
      (λ ()
        (void (with-handlers ([exn:break? (λ _ (displayln "User break"))])
                (for ([k (in-list keys)])
                  (procure k)
                  (log-unlike-assets-info "Procured ~a" k))
                (action))))
      #:logger unlike-assets-logger
      level
      'unlike-assets)))
