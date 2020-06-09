#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    unlike-assets]]

@title{Errors}

@defmodule[unlike-assets/resolver/exn]

@defstruct*[exn:fail:unlike-assets:unresolved ([name any/c] [dependents list?])]{
An error raised when a resolver could not produce a @tech{resolved name}.

@racket[name] is a reference to the exact name a user passed to a
resolver.

@racket[dependents] is a @tech{dependents list}. Assuming the name
resolution error did not occur at all, the value named by the first
element would be dependent on the resolved variant of @racket[name].

}

@defstruct*[exn:fail:unlike-assets:cycle ([scope procedure?] [dependency any/c] [dependents any/c])]{
An error raised when a @tech{resolver} or a thunk it produced
encounted a circular dependency.

@racket[scope] is a procedure that would never terminate if it weren't
for this exception.

@racket[dependency] is a @tech{resolved name} of a resource that is
already a dependent, and therefore cannot be resolved.

@racket[dependents] is a @tech{dependents list}, where the value named
by the first element is dependent on the value named by
@racket[dependency].

}


@defproc[(raise-name-resolution-error [name any/c] [dependents list?]) any]{
Raises @racket[exn:fail:unlike-assets:unresolved] with the given
@racket[name], a preformatted message, and @racket[(current-continuation-marks)].
}


@defproc[(raise-cycle-error [scope procedure?] [name any/c] [dependents list?]) any]{
Raises @racket[exn:fail:unlike-assets:cycle] with the given arguments,
a preformatted message, and @racket[(current-continuation-marks)].
}
