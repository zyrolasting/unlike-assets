#lang scribble/manual
@require[@for-label[unlike-assets racket racket/class graph]]

@title{Imperative Model}

The imperative model uses Stephen Chang's
@hyperlink["https://docs.racket-lang.org/graph/index.html"]{graph
library} in a simple object model to track dependencies between assets
and build them in order. This model is good for those who are used to
classical object-oriented programming and/or anyone who wants to work
on the underlying graph of their project as a whole.

To use this model, you subclass @racket[unlike-compiler%] and
override two methods called @method[unlike-compiler% clarify]
and @method[unlike-compiler% delegate].

@method[unlike-compiler% clarify] translates an string like @tt{styles.css} into an
unambiguous identifier like an absolute path or full URL.  What
counts as a clear or unclear name is up to you.

@method[unlike-compiler% delegate] takes a clear name and maps it to
the first procedure that must run to start building the asset of that
name.  That procedure can identify dependencies of your asset and add
them to the graph. So if you process an HTML file, then you can find
dependencies in that file and queue them up for further processing.

If an asset changes, you can also control how that change propagates
to other assets.

@deftogether[(
@defthing[unclear/c string?]
@defthing[clear/c (or/c string? symbol? url? path? (generic-instance/c gen:equal+hash))]
@defthing[fulfilled/c (not/c procedure?)]
@defthing[advance/c (recursive-contract
                      (-> clear/c (instanceof/c (subclass?/c unlike-compiler%)) unlike-asset/c))]
@defthing[unlike-asset/c (or/c advance/c fulfilled/c)]
)]{
Let's rehash the fundamentals as contracts set by the context of this model.

When an asset value (as per @racket[unlike-asset/c]) is fulfilled, it's not a
procedure and its value is final. Otherwise, it is an
@racket[advance/c] procedure that returns a new asset that is
fulfilled or unfulfilled.

An instance of @racket[unlike-compiler%] must @method[unlike-compiler%
clarify] these strings and @method[unlike-compiler% delegate] work to
procedures that can fulfill the assets under the @racket[clear/c]
names. An @racket[unlike-compiler%] instance can also mark changes on
an asset's value and control how that change ripples to dependencies.
}

@section{Imperative API Reference}

@defclass[unlike-compiler% object% ()]{
An abstract class that coordinates asset fulfillment.

Depending on your requirements and the complexity of your project, you may need to use custodians,
threads, engines, places, or other constructs to coordinate different instances of this class.

@defmethod[(clarify [unclear unclear/c]) clear/c]

Override this method to deterministically map an unclear string to a clear name for an asset.
By default, @method[unlike-compiler% clarify] is the identity function.

Once assets have clear names we need to decide what to do with them by delegating work out
to appropriate procedures.

@defmethod[(delegate [clear clear/c]) unlike-asset/c]{

Override this abstract method to deterministically return the
@bold{first} value to represent an asset of name @racket[clear].

@margin-note{If you want your terminal value to be a procedure, wrap it in a @racket[box], @racket[list], etc.}
If @method[unlike-compiler% delegate] returns an @racket[advance/c] procedure, that procedure must
accept the same clear name and the instance of the compiler as arguments, and either return the
@bold{next} @racket[advance/c] procedure to pass on responsibility, or a terminal value that
isn't a procedure at all.

Any procedure in the implied chain of fulfillment can (and should) @method[unlike-compiler% add!]
dependencies to the compiler as they are discovered. If this occurs, the
subsequent procedure will not be called until those dependencies are fulfilled.

Once clarified names can be used to delegate work to procedures, you can @method[unlike-compiler% compile!]
}

@defmethod[(compile! [#:changed changed (listof clear/c) null]
                     [#:removed removed (listof clear/c) null]
                     [#:strict? strict? any/c #t])
                     (hash/c clear/c fulfilled/c)]{
Fulfills all assets on the current thread and returns a hash mapping clear names to the final value
associated with each asset. Will raise @racket[exn:fail] if a call to @method[unlike-compiler% compile!] is already
running for the instance.

Side-effects:

@itemlist[
@item{The encapsulated model will record all asset activity.}
@item{Events are sent to @racket[unlike-asset-logger].}
]

If @racket[changed] or @racket[removed] are not empty, then the compiler will first modify
the underlying model to reflect changed or removed assets according to @secref["live"].
}


@defmethod[(lookup [clear clear/c])
                   unlike-asset/c]{
Return the current value associated with a clear name in the compiler.
Will raise @racket[exn:fail] if no asset is found.
}

@defmethod[(add! [clear clear/c] [dependent-clear (or/c clear/c boolean?) #f] [ripple (or/c ripple/c boolean?) #f]) void?]{
Adds a clear asset name to the compiler. If @racket[dependent-clear] is a clear name,
then the compiler will understand that @racket[clear] is a dependency of @racket[dependent-clear].
Will raise @racket[exn:fail] if a circular dependency forms.

@racket[ripple] controls how a change in @racket[clear]'s asset propagates to @racket[dependent-clear]'s asset.
By default, the dependent asset will be rebuilt. Otherwise the change will produce an asset value from a provided
@racket[ripple/c] procedure.

For information on the change model, see @secref["live"].
}
}

@include-section["rebuilds.scrbl"]
@include-section["cli.scrbl"]
@include-section["examples.scrbl"]
