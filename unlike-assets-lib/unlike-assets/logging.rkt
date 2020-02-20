#lang racket/base

;; Provides logging unique to this library for use internally and by the user.

(require racket/dict
         racket/function
         racket/logging
         racket/match
         racket/string
         ansi-color)

(provide (all-defined-out))

(define show-debug?       (make-parameter #f))              ; Controls if receiver processes debug level events
(define show-colors?      (make-parameter #f))              ; Use ANSI sequences to color output.
(define show-prefix?      (make-parameter #f))              ; Show topic prefix in log
(define show-all-events?  (make-parameter #f))              ; Show all topics and levels
(define show-level?       (make-parameter #f))              ; Show level prefix in log
(define error-port-levels (make-parameter '(fatal error)))  ; Levels that are directed to (current-error-port)
(define prescribed-prefix (make-parameter ""))              ; An enforced prefix used to track activity deep inside threads or other hard-to-track control structures.
(define format-clear      (make-parameter identity))        ; Procedure to format values returned from (current-clarify-unlike-proc)
(define format-unclear    (make-parameter identity))        ; Procedure to format values given to (current-clarify-unlike-proc)

;; Isolate the logger to enable explicit control over events.
(define TOPIC 'unlike-assets)
(define unlike-assets-logger (make-logger TOPIC #f))

(define (make-child-logger)
  (make-logger TOPIC unlike-assets-logger))

(define (<log level message [data #f])
  (define current (current-logger))
  (define target (if (equal? (logger-name current) TOPIC)
                     current
                     unlike-assets-logger))

  (log-message unlike-assets-logger
               level
               TOPIC
               message
               data
               (show-prefix?)))

;; Reinvent (define-logger) bindings here.
;; I didn't like how much happened out of my view.
(define (<log/cm level message . v )
  (<log level
        (apply format
               (cons (format "~a~a"
                             (prescribed-prefix)
                             message)
                     v))
        (current-continuation-marks)))

(define <fatal   (curry <log/cm 'fatal))
(define <error   (curry <log/cm 'error))
(define <info    (curry <log/cm 'info))
(define <debug   (curry <log/cm 'debug))
(define <warning (curry <log/cm 'warning))


;; These procedures are helpful for processing log events.
(define (level->color level)
  (case level
    [(fatal)   (values 0 1)]
    [(error)   (values 0 9)]
    [(warning) (values 0 11)]
    [(debug)   (values 0 13)]
    [else      (values 0 15)]))

(define (level->port level [stderr-levels (error-port-levels)])
  (if (member level stderr-levels)
      (current-error-port)
      (current-output-port)))

(define (report/colorfully level formatted)
  (define-values (bg fg) (level->color level))
  (parameterize ([background-color bg] [foreground-color fg])
    (color-display formatted (level->port level))))

(define (report level formatted)
  (display formatted (level->port level)))

(define (get-report-proc)
  (if (show-colors?)
      report/colorfully
      report))

(define (make-counts-dict)
  (foldl (λ (l h) (dict-set h l 0))
         (make-immutable-hash)
         '(debug info warning error fatal)))

(define (count-level dict level)
  (dict-set dict
            level
            (if (dict-has-key? dict level)
                (add1 (dict-ref dict level))
                1)))

(define (may-display-level? level topic)
  (if (equal? topic TOPIC)
    (if (equal? level 'debug)
        (or (show-all-events?) (show-debug?))
        #t)
    (or (show-all-events?)
        (member level ; Racket's info channel is noisy, but don't hide urgent messages.
                '(warning error fatal)))))

(define (format-message level message)
  (define level? (show-level?))
  (if level?
      (format "~a: ~a~n" level message)
      (format "~a~n" message)))

;; Sugary receiver that counts events by level
;; and displays the messages to stdout or stderr.
(define (with-report proc)
  (define counts (make-counts-dict))
  (define proc-out
    (with-intercepted-logging
      (match-lambda [(vector level message _ topic)
                     (when (may-display-level? level topic)
                       (set! counts (count-level counts level))
                       ((get-report-proc) level
                                          (format-message level
                                                          message)))])
      proc
      #:logger unlike-assets-logger
      'debug))
  (values proc-out counts))

(define (with-report/void proc)
  (with-report proc)
  (void))

(define (with-report/counts proc)
  (define-values (_ counts)
    (with-report proc))
  counts)
