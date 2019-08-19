#lang racket/base

(require
  racket/class
  racket/control
  racket/cmdline
  racket/dict
  raco/command-name
  "private/assets.rkt"
  "private/unlike-compiler.rkt"
  "logging.rkt"
  "policy.rkt")

(define spec-module-path     (make-parameter (build-path "?")))
(define initial-asset-paths  (make-parameter null))
(define ENOENT 2)
(define EINVAL 22)
(define EPROTO 71) ; For representing non-zero errors during build.
                   ; Is there a better fit?

(with-handlers ([string? displayln])
  (command-line
    #:program (short-program+command-name)
    #:usage-help
    "Compile <asset-refs> according to <policy-module>."
    "Run `raco doc unlike-assets` for documentation."
    #:once-each
    [("-v" "--verbose") ("Include debug level logs")
                        (show-debug? #t)]
    [("--noisy")        ("Include logs from all levels and topics. Implies -v.")
                        (show-all-events? #t)
                        (show-debug? #t)]
    [("-c" "--colors")  ("Include ANSI color sequences in output")
                        (show-colors? #t)]
    [("--show-prefix")  ("Include topic prefix in logs")
                        (show-prefix? #t)]
    [("--show-level")   ("Include level prefix in logs")
                        (show-level? #t)]
    #:args (policy-module . asset-refs)
    (begin
      (spec-module-path policy-module)
      (initial-asset-paths asset-refs))))


(define (load-compiler)
  (define (module-not-found e)
    (<fatal "Could not load policy module: ~a~n~a~n" (spec-module-path) e))
  (with-handlers ([exn:fail:filesystem? module-not-found])
    (dynamic-require (spec-module-path) 'compiler)))

(define (run-compiler compiler entries)
  (if (= (length entries) 0)
    (<fatal "No assets to process.")
    (begin
      (for ([clear (clarify/multi compiler entries)])
           (send compiler add! clear))
      (for ([(k v) (in-hash (send compiler compile!))])
           (<info "~a -> ~a" k v)))))

(define (build)
  (define compiler (load-compiler))
  (if (is-a? compiler unlike-compiler%)
    (run-compiler compiler (initial-asset-paths))
    (<fatal "Expected subclass of unlike-compiler%. Got ~a" compiler)))

(define (prepare-report)
  (define counts (with-report/counts build))
  (define nwarnings (dict-ref counts 'warning 0))
  (define nerrors (+ (dict-ref counts 'error 0)
                     (dict-ref counts 'fatal 0)))
  (with-report/void
    (λ ()
      (<info "# warnings: ~a" nwarnings)
      (<info "# errors:   ~a" nerrors)))

  (if (> nerrors 0) 1 0))

(exit (prepare-report))
