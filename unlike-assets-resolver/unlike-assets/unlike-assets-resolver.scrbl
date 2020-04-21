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
@racketmodname[unlike-assets/resolver/asset], and
@racketmodname[unlike-assets/resolver/global].

This section will cover each binding provided by all of these modules.

@section{@tt{unlike-assets/resolver/global}}
@defmodule[unlike-assets/resolver/global]

This module provides a process-wide interface for
@racketmodname[unlike-assets/resolver/base],
@racketmodname[unlike-assets/resolver/pod], and
@racketmodname[unlike-assets/resolver/asset]. You can create your own
resolver in terms of those modules.

@defthing[current-resolver (parameter/c resolver?)]{
This is a global resolver used by @racket[procure] to find updated
Racket values. By default, it raises an error that it cannot
resolve any key. You will need to provide an implementation by
setting this parameter, possibly with @racket[u/a].
}

@defproc[(procure/weak [key string?]) (-> asset?)]{
This starts an asynchronous build for an asset (if needed), but does
not wait for the result. Returns a procedure that will.
}

@defproc[(procure [key string?] [sym symbol?] ...) any]{
This starts a build for an asset (if needed), waits for the result,
then returns that asset.

You can optionally provide symbols to access keys inside
of the procured asset, in which case @racket[procure] will
return as many values as there are @racket[sym] arguments.
In this case, the asset itself is not returned.

@racketblock[
(define-values (size check-schema) (procure "data.json" 'size 'check-schema))
(define title (procure "article.doc" 'title))
]
}

@deftogether[(
@defthing[Pw procure/weak]
@defthing[P procure]
)]{
You'll probably use the @tt{procure/*} procedures often enough to want
these abbreviations.
}

@defform[(define-procured key id ...)]{

These are equivalent:

@racketblock[
(define-procured "data.json" size check-schema)
(define-values (size check-schema) (procure "data.json" 'size 'check-schema))
]
}

@defproc[(u/a [route route/c] ...) void?]{
Imperatively replaces @racket[current-resolver] with a new resolver
built with the given routes. This preserves the hash table used by the
existing resolver.
}

@section{@tt{unlike-assets/resolver/base}}
@defmodule[unlike-assets/resolver/base]

A resolver maps strings to @tech{pods}. Pods may use the same
resolver to depend on other pods.

@defproc[(resolver? [p any/c]) boolean?]{
Returns @racket[#t] if @racket[p] came from @racket[make-resolver].
}

@defthing[route/c contract? #:value (-> string? resolver? (or/c #f pod?))]{
Matches procedures that map strings to @tech{pods}.
}

@defproc[(make-resolver [#:known known (hash/c string? pod?) (hash)] [find-pod route/c] ...) resolver?]{
Returns a procedure @racket[R] that encapsulates a mutable copy
of @racket[known].

@racket[R]'s behavior depends on the number of provided arguments:

@itemlist[
@item{@racket[(R)] returns a copy of the internal hash.}

@item{@racket[(R key)] will return @racket[(hash-ref internal-hash
key)], or raise @racket[exn:fail:unlike-assets:cycle] if using the
referenced pod would create a cycle. If the key does not map to a pod,
then the key will be set to the first @racket[(find-pod key R)] to
return a pod. @racket[R] may be invoked recursively in any
@racket[find-pod].}

@item{@racket[(R key return?)]: Returns @racket[(or (return? P) (return? (P) ...))],
where @racket[P] is the @tech{pod} returned from @racket[(R key)]. Useful for
finding a common value type. Remember that due to the nature of pods, @racket[(R key return?)]
is non-deterministic.
}

@item{@racket[(R key return? make-alias)]: Like @racket[(R key
return?)].  As a side effect, it adds a key equal to
@racket[(make-alias key (R key return?))] to the internal hash table.
Useful for finding pods based on values they produced.}
]
}

@defstruct[exn:fail:unlike-assets:cycle ([dependency-key string?] [dependent-keys (listof string)])]{
An error raised when the resolver encounters a cycle.

@racket[dependency-key] is the string key used to resolve a pod that exists in @racket[dependent-keys].

@racket[dependent-keys] is the list of keys used to resolve pods
leading up to @racket[dependency-key].

The pod with the first key in @racket[dependent-keys] is dependent on
the pod with the @racket[dependency-key]. Beyond that, the pod with
the @racket[N]th key in @racket[dependent-keys] is dependent on the
pod with the @racket[N-1]th key.
}

@defproc[(invert-found [R resolver?]) (hash/c pod? (non-empty-listof string?) #:immutable #t)]{
Returns a @racket[hasheq] hash that represents the inverse of
@racket[(R)], meaning each key is a pod and each value is a list
of keys used to refer to the respective pod.
}

@defproc[(in-found [R resolver?]
                   [return? predicate/c pod?]
                   [keep? (-> any/c (non-empty-listof string?) any/c)
                   (const #t)]) sequence?]{
Returns a two-value sequence filtered by @racket[keep?].

The first value is equal to @racket[(R key return?)].
The second value is a list of all keys that can be used to
access the first value using @racket[R].

If @racket[return?] is @racket[pod?], then @racket[(R key)] is
@racket[eq?] to the first sequence value for each @racket[key] in the second.
Therefore, @racket[(in-found R pod? (const #t))] is equivalent to
@racket[(in-hash (invert-found (R)))].
}



@section{@tt{unlike-assets/resolver/pod}}
@defmodule[unlike-assets/resolver/pod]

@deftech{Pods} keep Racket values up-to-date, asynchronously.

@racketmodname[unlike-assets/resolver/pod] reprovides all bindings
from @racketmodname[unlike-assets/resolver/fence].

@defproc[(pod? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] came from @racket[make-pod].
}

@defproc[(make-pod [key string?] [make-build (-> (or/c #f (-> any/c)))]) pod?]{
Returns a procedure @racket[build!] that governs a changing
value. Initially, that value is @racket[undefined].

@racket[(build!)] starts a build (if needed) and returns a
@racket[wait-for-result] procedure that does what it says.

Overall, use of a pod looks like this.

@racketblock[
(define build! (make-pod ...))
(define wait-for-result (build!))
(define result (wait-for-result))
]

@racket[(build!)] will apply @racket[make-build] in search of a
procedure to use to create a singular Racket value.

If @racket[make-build] returns a procedure, then @racket[build!] will
run the returned procedure in a new thread.  If a thread is already
running for the pod, then that thread will be sent a break before
being replaced. A continuation mark with key @racket['dependent-pods]
will be set in the dynamic extent of a pod's thread. If @racket['dependent-pods]
is not mapped to a value, it will be set to @racket[(list key)]. Otherwise,
it will be set to @racket[(cons key existing-value)].

If @racket[make-build] returns @racket[#f], then @racket[build!] has no
side-effects. Specifically, @racket[(build!)] will still return a wait
procedure, but it will not break any existing build threads, and it
will not create a new thread.

Here's an example that returns the string contents of a file, where
@racket[file->string] runs without blocking the current thread, but
only if the file exists.

@racketblock[
(define build!
  (make-pod "foo"
            (lambda () (and (file-exists? "/home/me/data")
                            (lambda () (file->string "/home/me/data"))))))
]

The wait procedures returned by @racket[build!] are always
@racket[eq?], meaning that it's the same procedure. It will always
return the @italic{latest} result from the build.

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

@defform[(asset/c pair ...)
         #:grammar [(pair [id contract-expr])]]{
Creates a contract that captures individual values in an asset.

@racketblock[
(asset/c [media-type bytes?]
         [writer (-> output-port? any)])]
}

@defform[(asset/p id ...)]{
A @racket[match] pattern for assets, @italic{not} the hashes they
decorate. You do not have to list every key for an asset, but every key
you list must exist in the asset's hash for the pattern to match.

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
