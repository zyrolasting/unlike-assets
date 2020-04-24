#lang racket/base

(provide u/a-cli)
(require racket/cmdline
         racket/logging
         racket/tcp
         raco/command-name
         "../resolver.rkt"
         "../files.rkt"
         "server.rkt"
         "distributor.rkt")

(define (u/a-cli)
  (define port 8080)
  (define level 'info)
  (define yes #f)
  (define-logger unlike-assets)

  (define (run-server/wait)
    (printf "Listening on port ~a~n" port)
    (displayln "Stop the server with ^C")
    (define stop (start-server #:port port))
    (dynamic-wind void
                  (λ () (sync/enable-break never-evt))
                  stop))

  (define (confirm prompt)
    (or yes
        (begin
          (printf "~a [y/N]: " prompt)
          (flush-output)
          (case (read-char)
            [(#\y #\Y) #t]
            [else #f]))))

  (define action
    (λ () (displayln "No action specified. Run again with -h to see options.")))

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
   [("-y" "--yes")
    "Answer yes to any prompts."
    (set! yes #t)]
   [("-v" "--verbose")
    "Show debug level logs"
    (set! level 'debug)]
   #:once-any
   [("-s" "--server")
    "Start server"
    (set! action run-server/wait)]
   [("-d" "--distribute")
    "Distribute files"
    (begin
      (displayln "WARNING: Distributing files is a destructive operation.")
      (displayln "By default, this will perform a dry run.")
      (displayln "(You can disable this prompt in the future with --yes)")
      (define disable-dry-run? (confirm "Disable the dry run and distribute the files?"))
      (set! action
            (λ () (sync-filesystem-to-resolved! #:dry-run? (not disable-dry-run?)))))]
    #:args keys
    (with-logging-to-port
      (current-output-port)
      (λ ()
        (void (with-handlers ([exn:break? (λ _ (displayln "User break"))])
                (unless (null? keys)
                  (log-unlike-assets-info "Requesting ~a asset(s)" (length keys)))
                (map Pw keys)
                (for ([k (in-list keys)])
                  (Ps k)
                  (log-unlike-assets-info "Procured ~a" k))
                (action))))
      #:logger unlike-assets-logger
      level
      'unlike-assets)))
