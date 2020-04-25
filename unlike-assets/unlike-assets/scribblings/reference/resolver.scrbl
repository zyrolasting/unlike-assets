#lang scribble/manual

@require[@for-label[racket/base
                    racket/bool
                    racket/contract
                    racket/file
                    racket/function
                    racket/match
                    racket/undefined
                    racket/rerequire
                    unlike-assets/resolver]]

@title{@tt{unlike-assets/resolver}: Reference}
@author{Sage Gerard}

@defmodule[unlike-assets/resolver]

@racketmodname[unlike-assets/resolver] provides all bindings from
@racketmodname[unlike-assets/resolver/base],
@racketmodname[unlike-assets/resolver/pod],
@racketmodname[unlike-assets/resolver/fence],
@racketmodname[unlike-assets/resolver/cycle],
@racketmodname[unlike-assets/resolver/asset], and
@racketmodname[unlike-assets/resolver/default].

This section will cover each binding provided by all of these modules.

@section{Summary of Model}
The intended experience of an Unlike Assets resolver is to provide a
@racket[(-> string? any/c)] variant of @racket[require] (e.g.
@racket[procure]). That is, a import procedure that is not limited to
@racket[require]'s data formats and conventions.  In this light, a
non-Racket resource can present itself as a software module.

In the context of this collection, an @deftech{asset} is a procedure
that returns an immutable hash of possible data points, or the value
of some data point.

This isn't particularly exciting until we can guarentee that assets
will be kept up-to-date. For that, I define @deftech{pods} as
procedures that always return the latest version of their assigned
asset. Pods optionally collaborate with @deftech{fences} to
decide if they should compute a new asset.

Finally, a @deftech{resolver} is a procedure that maps strings to
pods.  Asset procurement is a runtime concern and there are an unknown
number of steps from a name to a concrete value. For that reason, the
resolver can iteratively apply thunks in search of a particular value
type. Using the default resolver, that value type matches
@racket[asset?].

You, the user, may either configure the default resolver or assemble
your own from these building blocks.

@section{@tt{unlike-assets/resolver/default}}
@defmodule[unlike-assets/resolver/default]

This module provides a process-wide interface for
@racketmodname[unlike-assets/resolver/base],
@racketmodname[unlike-assets/resolver/pod], and
@racketmodname[unlike-assets/resolver/asset]. You can create your own
resolver in terms of those modules, hence why this one is called @tt{default}.

@defthing[current-resolver (parameter/c resolver?)]{
This is a global resolver used by @racket[procure] to find updated
Racket values. By default, it raises an error that it cannot
resolve any key. You will need to provide an implementation by
setting this parameter, possibly with @racket[u/a].
}

@defproc[(procure [key string?]) asset?]{
Returns an asset using a string key, with the understanding that the
asset contains up-to-date information. This implies that
@racket[procure] is non-deterministic.

As a side-effect, @racket[procure] logs the procure action to the
@racket['unlike-assets] topic on the @racket['debug] level.

@racket[procure] will obtain a @tech{pod} @racket[P] using
@racket[((current-resolver) key)], and then apply @racket[P] and any
results iteratively until it obtains an asset.  This behavior is
equivalent to the following:

@racketblock[
(let loop ([v ((current-resolver) key)])
  (if (asset? v) v
      (loop (v))))]

It behaves this way to account for value types that contain several
layers of indirection to obtain a concrete value. If @racket[v] never
produces an asset and is always a procedure, then @racket[procure]
will not terminate.
}

@defproc[(u/a [route route/c] ...) void?]{
Imperatively replaces @racket[current-resolver] with a new resolver
built with the given routes. This preserves the hash table used by the
existing resolver.
}

@defproc[(in-assets [R resolver?]
                   [keep? (-> any/c (non-empty-listof string?) any/c)
                   (const #t)]) sequence?]{
Returns a two-value sequence filtered by @racket[keep?].

The first value is an @tech{asset}. The second value is a list of all
keys that can be used to access that asset using @racket[procure].

This is useful for acting upon visited assets.
}


@section{@tt{unlike-assets/resolver/base}}
@defmodule[unlike-assets/resolver/base]
A resolver maps strings to @tech{pods}. Pods may use the same
resolver to depend on other pods.

@defproc[(resolver? [p any/c]) boolean?]{
Returns @racket[#t] if @racket[p] came from @racket[make-resolver].
}

@defthing[route/c contract? #:value (-> string? resolver? (or/c #f pod?))]{
Matches procedures that map strings to @tech{pods}, or @racket[#f].

@racket[#f] means that no pod applies for a given string.
}

@defproc[(make-resolver [known (hash/c string? pod?)]
                        [key->pod (-> string? pod?)]
                        [resolved? predicate/c])
                        (and/c resolver?
                               (case-> (-> (hash/c pod?
                                                   (non-empty-listof string?)
                                                   #:immutable #t))
                                       (-> string? pod?)))]{
Returns a procedure @racket[R] that encapsulates a mutable copy of
@racket[known]. @racket[R]'s behavior depends on the number of
provided arguments.

@racket[(R key)] binds @racket[key] to racket[(key->pod key
R)], if the key is not already set. Returns @racket[(or
(resolved? P) (resolved? (P)) ...)], where @racket[P] is the
pod bound to @racket[key].

If @racket[key->pod] does not return a pod, then @racket[R] will raise
@racket[exn:fail:contract]. If @racket[key->pod] applies @racket[R] to
in a way that forms a circular dependency, then that application of
@racket[R] will raise @racket[exn:fail:unlike-assets:cycle].

@racket[(R)] returns a hash @racket[H] that contains all values and
known names encountered over the life of the resolver. @racket[H] is
useful for reviewing visited values, while knowing which keys mapped
to the same value.

@racketblock[
(R "a") (R "b") (R "c")
(R) (code:comment "#hash((#<x> . '("a" "b")) (#<y> . '("c")))")
]
}

@defstruct[exn:fail:unlike-assets:cycle ([dependency-key string?] [dependent-keys (listof string)])]{
An error raised when a resolver encounters a cycle.

@racket[dependency-key] is the string key for the offending request,
that already exists in @racket[dependent-keys].

@racket[dependent-keys] is the list of keys used for some resolver
leading up to @racket[dependency-key], where the first element is the
most recent request. Formally, the pod with the first key in
@racket[dependent-keys] is dependent on the pod with key
@racket[dependency-key]. Beyond that, the pod with the @racket[N]th
key in @racket[dependent-keys] is dependent on the pod with the
@racket[N-1]th key.
}

@section{@tt{unlike-assets/resolver/pod}}
@defmodule[unlike-assets/resolver/pod]

Pods keep Racket values up-to-date.

@racketmodname[unlike-assets/resolver/pod] reprovides all
bindings from @racketmodname[unlike-assets/resolver/fence].

@defproc[(pod? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] came from @racket[make-pod].
}

@defproc[(make-pod [key string?] [make-build (-> (or/c #f (-> any/c)))]) pod?]{
Returns a procedure @racket[build!] that governs a changing
value. Initially, that value is @racket[undefined].

@racket[(build!)] starts a build (if needed) and returns a
@racket[get-result] procedure that does what it says.

@racketblock[
(define build! (make-pod ...))
(define get-result (build!))
(define result (get-result))
]

@racket[(build!)] applies @racket[make-build] in search of a procedure
to use to create a singular Racket value. If @racket[make-build]
returns a procedure, then @racket[build!] will apply that procedure on
the current thread and cache its value.

If @racket[make-build] returns @racket[#f], then @racket[build!] has
no side-effects. That is, @racket[(build!)] will still return a
procedure to get the cached result, but will not replace that result.

Here's an example that returns the string contents of a file,
provided the file exists.

@racketblock[
(define build!
  (make-pod (lambda () (and (file-exists? "/home/me/data")
                            (lambda () (file->string "/home/me/data"))))))
]

The wait procedures returned by @racket[build!] are always
@racket[eq?]. It will always return the @italic{latest} result.

@racketblock[
(define build! (make-pod "" (const (lambda () (current-seconds)))))
(define get-seconds (build!))
(get-seconds) (code:comment "1587437062")
(build!) (code:comment "don't bind again")
(code:comment "wait a few seconds...")
(get-seconds) (code:comment "1587437070")
]

Finally, any values raised during a build will be caught and
re-raised when you wait for them.

@racketblock[
(define build! (make-pod "" (const (lambda () (error "uh-oh")))))
(define wait (build!))
(wait) (code:comment "Exception re-raised here")
]
}

@defproc[(make-pod/fenced [key string?] [build? (-> any/c)] [build (-> any/c)]) pod?]{
Like @racket[make-pod], except @racket[build?] controls whether
the pod uses @racket[build] to replace its value.

@racket[(make-pod/fenced key build? build)] is equivalent to:

@racketblock[
(define build! (make-pod key (lambda () (and (build?) build))))
]
}

@defform*[(
(pod key (=* fexp ...) body ...)
(pod key body ...)
)]{
Creates a pod with an optional use of @racket[fence].

@racketblock[
(define build!
  (pod key (=* (file-or-directory-modify-seconds path))
       (file->string path)))

(code:comment "expands to:")
(define build!
  (make-pod/fenced key
    (fence (file-or-directory-modify-seconds path))
    (lambda () (file->string path))))
]

In this example, @racket[file-or-directory-modify-seconds] will not
evaluate unless you apply @racket[build!]. And even then,
@racket[file->string] will not evaluate unless the fence thunk
returns true. See @racket[fence] for details.

Without a fence expression, @racket[(pod key body ...)] expands
to @racket[(pod key (=* #f) body ...)]. Such pods only build
their values once.
}

@section{@tt{unlike-assets/resolver/asset}}
@defmodule[unlike-assets/resolver/asset]

Assets are just hashes with lipstick.

@defproc[(make-asset [h (and/c immutable? hash?)]) procedure?]{
Returns a procedure @racket[P] such that:

@itemlist[@item{@racket[(P)] returns @racket[h].}
          @item{@racket[(P k)] returns @racket[(hash-ref h k)].}
          @item{@racket[(P k t)] returns @racket[(hash-ref h k t)].}]
}

@defproc[(asset? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a value produced by @racket[make-asset].
}

@defform[(asset pair ...)
         #:grammar ([pair [id expr]])]{
A macro that expands to a @racket[make-asset] call.

These two expressions are equivalent:

@racketblock[
(asset [media-type #"text/plain"]
       [nums '(1 3)])]

@racketblock[
(make-asset (hash 'media-type #"text/plain"
                  'nums '(1 3)))]
}

@defproc[(merge-assets [#:combine/key combine/key procedure? (lambda (k v0 v) v)] [a asset?] ...) asset?]{
Like @racket[hash-union], except assets are merged in order.  By
default, key conflicts are resolved by selecting the value from the
value belonging to last asset among the arguments with that key.
}

@defform[(asset/c pair ...)
         #:grammar [(pair [id contract-expr])]]{
Creates a contract that captures individual values in an asset.

@racketblock[
(asset/c [media-type bytes?]
         [writer (-> output-port? any)])]
}

@defform[(asset/p id ...)]{
A @racket[match] pattern for assets procedures. You do not have to
list every key for an asset's underlying hash, but every key you list
must exist in that hash for the pattern to match.

@racketblock[
(match-define (asset/p flavor) (asset [flavor 'chocolate] [kind 'ice-cream]))
(eq? flavor 'chocolate)

(match-define (asset/p vendor) (asset [flavor 'chocolate] [kind 'ice-cream]))
(code:comment "match-define: no matching clause for #<procedure:asset>")
]
}

@section{@tt{unlike-assets/resolver/fence}}
@defmodule[unlike-assets/resolver/fence]

Fence thunks detect change. While fence thunks cooperate well with
@racket[make-pod/fence], you can use them to test if you should react
to some external circumstance.

Fence thunks are meant to track the impact of side-effects. They can
consider raised values as a sign of change as opposed to a reason to
halt a program, which is relevant for managing pods.

@defform[(fence body ...)]{
Expands to:
@racketblock[
(make-fence-thunk (λ () body ...) equal?/raised #:capture #t)
]
}

@defproc[(make-fence-thunk [sample (-> any/c)]
                           [#:capture? capture? any/c #f]
                           [same? (or/c (-> any/c any/c boolean? boolean? any/c)
                                        (-> any/c any/c any/c))
                                  (if capture? equal?/raised equal?)])
                                  (-> boolean?)]{
Returns a thunk @racket[F]. Semantically, if @racket[(F)] is
@racket[#t], you can assume that something has changed according to
@racket[sample] and @racket[same?].

@racket[make-fence-thunk] immediately evaluates @racket[(sample)] and
remembers it's value. The latest value of @racket[(sample)] will be
cached on an ongoing basis. The first application of @racket[F] will
always return @racket[#t] to capture the change from no value to the
first cached value.

Every time you apply @racket[F] after the first time, it will apply
@racket[sample] and compare the returned value to the value last seen
using @racket[same?]. If @racket[(same? ...)] is @racket[#f], then
@racket[(F)] returns @racket[#t].

If @racket[capture?] is true, then any value @racket[raise]d from
@racket[sample] will be treated as the output of @racket[(sample)]
(but that value will be flagged as raised to aid comparisons).

@racket[same?] can accept either two or four formal arguments.  If
two, then the arguments will just be the value from a call to
@racket[(sample)], and the value from a subsequent call to
@racket[(sample)], in that order. If @racket[same?] accepts four
arguments, the latter two arguments are just booleans. The first
boolean is @racket[#t] if the first argument was raised from the body
of @racket[sample].  The second boolean is @racket[#t] if the same
applies to the second argument.  If @racket[capture?] is @racket[#f],
then the boolean arguments will always be @racket[#f].

@racketblock[
(define (same? prev-sample next-sample prev-raised? next-raised?)
  (when (and (not prev-raised?) next-raised?)
    (displayln "An error appeared"))
    ...)
]

}

@defproc[(equal?/raised [a any/c] [b any/c] [a-raised? boolean?] [b-raised? boolean?]) any/c]{
Like @racket[equal?], except the result is @racket[#f] if @racket[(xor a-raised? b-raised?)].

Also, if @racket[a] and @racket[b] were both raised and are both
exceptions, @racket[(equal?/raised a b #t #t)] is true if @racket[a]
is @racket[eq?] to @racket[b], or if @racket[(equal? (exn-message a)
(exn-message b))] is true.}
