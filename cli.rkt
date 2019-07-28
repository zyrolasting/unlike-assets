#lang racket/base

;;; Translates a command line to a compilation.

(require
  racket/cmdline
  racket/dict
  raco/command-name
  "private/assets.rkt"
  "private/compiler.rkt"
  "logging.rkt")

(define spec-module-path     (make-parameter (build-path "?")))
(define initial-asset-paths  (make-parameter null))
(define exitcode             (make-parameter 0))
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

(define (module-not-found e)
  (<fatal "Could not load policy module: ~a~n~a" (spec-module-path) e)
  (exitcode ENOENT))

(define (compile-single clarify resolve entries)
  (define entry (list-ref entries 0))
  (with-handlers ([exn? <error])
    (compile-unlike clarify resolve entry)
    (<info "Finished ~a" entry)))

(define (compile-multiple clarify resolve entries)
  (for ([(k v)
         (in-dict (compile-all-unlike clarify resolve entries))])
        (if (exn? v)
            (<error v)
            (<info "Finished ~a" k))))

(define counts
  (with-report/counts
    (λ ()
      (<debug "Policy module: ~s" (spec-module-path))
      (<debug "Initial assets: ~s" (initial-asset-paths))

      (define entries (initial-asset-paths))
      (define (import-from-policy sym)
        (with-handlers ([exn:fail:filesystem? module-not-found])
          (dynamic-require (spec-module-path) sym)))

      (define resolve (import-from-policy 'resolve))
      (define clarify (import-from-policy 'clarify))

      (case (length entries)
        [(0)
          (<fatal "No assets to process.")
          (exitcode ENOENT)]
        [(1) (compile-single clarify resolve entries)]
        [else (compile-multiple clarify resolve entries)]))))

(define nwarnings (dict-ref counts 'warning 0))
(define nerrors (+ (dict-ref counts 'error 0)
                   (dict-ref counts 'fatal 0)))

(with-report/void
  (λ ()
    (<info "# warnings: ~a" nwarnings)
    (<info "# errors:   ~a" nerrors)))

(when (= (exitcode) 0)
  (exitcode (if (> nerrors 0) EPROTO 0)))

(exit (exitcode))
