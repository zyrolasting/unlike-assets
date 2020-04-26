#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    unlike-assets/resolver]]

@title{@tt{unlike-assets/resolver/default}}
@defmodule[unlike-assets/resolver/default]

The default resolver produces @racket[hash-eq?] hashes, and provides
decorators to extend different parts of its functionality.

@defform[(define-hasheq-extension id [key-id contract-expr] ...)]{
For top level use only. Expands to several definitions that aids
contruction of @racket[hash-eq?] hashes with uninterned symbols
for keys.

Defines each @racket[key-id] as an uninterned symbol with the same content.

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