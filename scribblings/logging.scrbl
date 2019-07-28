#lang scribble/manual

@title{Logging}

@defmodule[unlike-assets/logging]

The behavior of @racket[unlike-assets] is difficult to trace without logging, hence this module.
You should use this module with your own build process to ensure consistent output, especially
when using the CLI.

@section{Logger interface}

@defthing[unlike-assets-logger logger?]{
This logger uses the @racket['unlike-assets] topic, has no parent, and subscribes to all levels of detail.}


@deftogether[(
@defproc[(<fatal [message-fmt string?] [v any/c] ...) void?]
@defproc[(<error [message-fmt string?] [v any/c] ...) void?]
@defproc[(<info [message-fmt string?] [v any/c] ...) void?]
@defproc[(<debug [message-fmt string?] [v any/c] ...) void?]
@defproc[(<warning [message-fmt string?] [v any/c] ...) void?]
)]{
Like @racket[<log/cm] on the named levels.
This is what you will use most of the time to log formatted messages to @racket[unlike-assets-logger].}


@defproc[(<log/cm [level log-level/c]
                  [message string?]
                  [v any/c] ...)
          void?]{
Equivalent to:

@racketblock[
(<log level
      (apply format (cons message v))
      (current-continuation-marks))
]
}

@defproc[(<log [level log-level/c]
               [message string?]
               [data any/c])
               void?]{
Equivalent to:

@racketblock[
(log-message unlike-assets-logger
             level
             'unlike-assets
             message
             data
             "unlike-assets")
]
}

@section{Building reports}

@defproc[(with-report [proc (-> any/c)]) (values any/c dict?)]{
Calls @racket[proc] such that any log messages sent to @racket[unlike-logger]
are intercepted and forwarded to the printer. Messages are customized using
several parameters. See @secref{params}.

Returns the value returned from @racket[proc], and a dictionary holding counts
for the number of each event encountered during evaluation of @racket[proc].
If no events are captured for a level, the dictionary will still hold the level
as a key with a value of @racket[0].}


@defproc[(with-report/void [proc (-> any/c)]) void?]{
Like @racket[with-report], except the return values are discarded.}


@defproc[(with-report/counts [proc (-> any/c)]) dict?]{
Like @racket[with-report], except only the event counts dictionary is returned.}


@section[#:tag "params"]{Logging Display Parameters}

@defthing[show-debug? (parameter/c boolean?) #:value #f]{
Informs receivers if @racket['debug] events should be displayed to the end user.}


@defthing[show-colors? (parameter/c boolean?) #:value #f]{
If @racket[#t], log messages will include ANSI color codes.}


@defthing[show-prefix? (parameter/c boolean?) #:value #f]{
If @racket[#t], the logger topic @racket["unlike-assets: "] will prefix each log message.}


@defthing[show-all-events? (parameter/c boolean?) #:value #f]{
If @racket[#t], the logger will include all events in end-user output, not just those for the @racket['unlike-assets] topic.

Combined with @racket[(show-debug? #t)], this can be extremely noisy. Use only if detailed feedback from Racket internals matter for your purposes.}


@defthing[show-level? (parameter/c boolean?) #:value #f]{
If @racket[#t], prefix each log message with the message's level of detail.}


@defthing[error-port-levels (parameter/c (listof log-level/c)) #:value '(fatal error)]{
Controls which levels are forwarded to @racket[(current-error-port)] be default. Typically you would
change this if you want to (not) count warnings as errors or constrict STDOUT to a single level without
using another process.}


@deftogether[(
@defthing[format-clear (parameter/c (-> clear/c any/c)) #:value identity]
@defthing[format-unclear (parameter/c (-> string? any/c)) #:value identity])]{
These procedures prepare clear and unclear dependency references for placement in a log message.
The output will be formatted in @racket[display] mode.}
