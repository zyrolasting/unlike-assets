#lang scribble/manual

@require[@for-label[racket/base
                    racket/file
                    racket/contract
                    unlike-assets
                    unlike-assets/resolver/default/extension]]

@title{@tt{unlike-assets/resolver/default/extension}}
@defmodule[unlike-assets/resolver/default/extension]

This module provides utility forms and procedures used to extend the
default resolver.

@defform[(define-hasheq-extension id [key-id contract-expr] ...)]{
For top level use only. Expands to several definitions that aids
contruction of @racket[hash-eq?] hashes with uninterned symbols
for keys.

Defines each @racket[key-id] as an uninterned symbol with the same
content.

Defines @italic{and provides} the following bindings:

@itemlist[
@item{@tt{id}: A constructor procedure that accepts as many arguments
as there are @racket[key-id]s. Those arguments must pass their given
contracts, and the argument positions follow the use of
@racket[define-hash-extension]. Returns a @racket[hash-eq?] hash.}

@item{@tt{id?}: A predicate that recognizes values returned from @tt{(id ...)}.}

@item{@tt{id-key-id}: A getter procedure that extracts the value bound to the associated key in a hash.}
]

The intended experience is as follows:

@itemlist[
@item{Create contracted definitions for content like @racket[struct],
but without a heirarchial relationship.}

@item{Allow users to combine different definitions with
@racket[hash-union], so that they can "decorate" a data object with
support for different consumers of the object.}

@item{Ensure no data is lost when using @racket[hash-union].}
]

The drawback is that these hashes may appear to have duplicate keys
when printed, because of how uninterned symbols work. But the benefit
of uninterned symbols is that they are unambiguously @racket[eq?] in
the eyes of their producers.

Because the uninterned symbols are not provided to client modules,
they can only access the hash values using getters, and can only
construct complete copies of each hash using the constructor
procedure.

Example:

@racketblock[
(define-hasheq-extension publishable
                         [due-date date?]
                         [send-to-editor (-> status/c)])

(define-hasheq-extension serveable
                         [handler (-> request? response?)])

(define-hasheq-extension serveable2
                         [handler (-> request? response?)])


(define p (publishable (date ...) (lambda () ...)))
(define s (serveable (lambda (req) ...)))
(define s2 (serveable2 (lambda (req) ...)))

(publishable-due-date p)
((publishable-send-to-editor p))

(define smush (hash-union p s s2))
(not (eq? (serveable-handler smush) (serveable2-handler smush)))
]
}



@defproc[(make-fence-thunk [make (-> any/c)] [same? (-> any/c any/c any/c) equal?]) boolean?]{
Returns the following caching procedure:

@racketblock[
(let ([cache (make)])
  (λ ([next (make)])
    (begin0 (not (same? cache next))
      (set! cache next))))]

The procedure returns @racket[#t] if any two successive values of
@racket[(make)] differ.
}

@defproc[(make-factory-thunk [make? (-> any/c)] [make (-> any/c)]) (-> any/c)]{

Returns the following procedure.

@racketblock[
(let ([result #f])
  (λ ()
    (when (make?) (set! result (make)))
    result))]

When applied, returns the value currently bound to @racket[result].

Useful with @racket[(make-fence-thunk)].
}


@defform[(fenced-factory fence factory)]{
This...

@racketblock[
(fenced-factory (file-or-directory-modify-seconds path
                (file->string path)))]

...expands to this.

@racketblock[
(make-factory-thunk (make-fence-thunk (lambda () (file-or-directory-modify-seconds path)))
                    (lambda () (file->string path)))]
}
