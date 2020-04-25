@defproc[(in-assets [R resolver?]
                    [keep? (-> any/c (non-empty-listof string?) any/c)
                    (const #t)]) sequence?]{
Returns a two-value sequence filtered by @racket[keep?].

The first value is an @tech{asset}. The second value is a list of all
keys that can be used to access that asset using @racket[procure].

This is useful for acting upon visited assets.
}

@racketmodname[unlike-assets/resolver/pod] reprovides all
bindings from @racketmodname[unlike-assets/resolver/fence].

@defproc[(pod? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] came from @racket[make-pod].
}

@defproc[(make-pod [make-build (-> (or/c #f (-> any/c)))]) pod?]{

Returns the functional equivalent of the following:

@racketblock[
(let ([make-build (λ () ...)]
      [result undefined])
   (λ ()
    (define build (make-build))
    (when build (set! result (build)))
    result))]

In more words, this returns a procedure @racket[build!] that governs a
cached value. Initially, that value is
@racket[undefined]. @racket[(build!)] applies @racket[make-build] in
search of a procedure to use to create a singular Racket value. If
@racket[make-build] returns a procedure, then @racket[build!] will
replace the cached value with the output of that procedure.

If @racket[make-build] returns @racket[#f], then @racket[build!]
will not update the cache.

Here's an example that returns the string contents of a file,
provided the file exists.

@racketblock[
(define build!
  (make-pod (lambda () (and (file-exists? "/home/me/data")
                            (lambda () (file->string "/home/me/data"))))))
]

@racketblock[
(define build! (make-pod (const (lambda () (current-seconds)))))
(build!) (code:comment "1587437062")
(code:comment "wait a few seconds...")
(build!) (code:comment "1587437070")
]
}

@defproc[(make-pod/fenced [build? (-> any/c)] [build (-> any/c)]) pod?]{
Returns @racket[(make-pod (lambda () (and (build?) build)))].
}

@defform*[(
(pod (=* fexp ...) body ...)
(pod body ...)
)]{
Creates a pod with an optional use of @racket[fence].

@racketblock[
(define build!
  (pod (=* (file-or-directory-modify-seconds path))
       (file->string path)))

(code:comment "expands to:")
(define build!
  (make-pod/fenced key
    (fence (file-or-directory-modify-seconds path))
    (lambda () (file->string path))))
]

In this example, @racket[file-or-directory-modify-seconds] will not
evaluate unless you apply @racket[build!]. And even then,
@racket[file->string] will not evaluate unless the @racket[fence]
condition flags a cache miss.

@racket[(pod body ...)] expands to @racket[(pod (=* #f) body
...)], which retains a constant value.
}

@section{@tt{unlike-assets/resolver/asset}}
@defmodule[unlike-assets/resolver/asset]

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
A @racket[match] pattern for asset procedures. You do not have to
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
always return @racket[#t] to represent the change from no value to the
first cached value.

Every time you apply @racket[F] after the first time, it will
return @racket[(not (same? ...))] with arguments corresponding
to the two most recent applications of @racket[sample].

If @racket[capture?] is true, then any value @racket[raise]d from
@racket[sample] will be caught and compared using @racket[same?].

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
