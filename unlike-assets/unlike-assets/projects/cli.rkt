#lang racket/base

(provide u/a-cli)
(require racket/cmdline
         racket/logging
         unlike-assets/resolver
         unlike-assets/files
         raco/command-name
         "server.rkt"
         "distributor.rkt")

(define (u/a-cli)
  (define action start-server)
  (define level 'info)
  (define yes #f)

  (define (confirm prompt)
    (or yes
        (let loop ()
          (printf "~a [y/N]: " prompt)
          (case (read-char)
            [(#\y #\Y) #t]
            [else #f]))))

  (command-line
   #:program (short-program+command-name)
   #:usage-help
   "Compile <asset-refs> according to <policy-module>."
   "Run `raco doc unlike-assets` for documentation."
   #:once-each
   [("-y" "--yes") ("Answer yes to any prompts.")
                   (set! yes #t)]
   [("-v" "--verbose") ("Show debug level logs")
                       (set! level 'debug)]
   #:once-any
   [("-s" "--server")
    "Start server"
    (set! action start-server)]
   [("-d" "--distribute")
    dir
    "Distribute files"
    (begin
      (displayln "WARNING: Distributing files is a destructive operation.")
      (displayln "By default, this will perform a dry run.")
      (define dry-run? (confirm "Disable the dry run and distribute the files?"))
      (displayln "You can disable this prompt in the future with --yes.")
      (set! action
            (λ () (sync-filesystem-to-resolved! #:dry-run? dry-run?))))]
    #:args keys
    (with-logging-to-port
      (current-output-port)
      (λ ()
        (with-handlers ([exn:break? (λ _ (displayln "User break"))])
          (map Pw keys)
          (action)))
      #:logger (current-logger)
      level 'unlike-assets)))
