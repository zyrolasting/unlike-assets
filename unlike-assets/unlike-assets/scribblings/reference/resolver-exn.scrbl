#lang scribble/manual

@require[@for-label[racket/base
                    racket/contract
                    racket/file
                    unlike-assets]]

@title{Errors}

@defmodule[unlike-assets/resolver/exn]

@defstruct*[exn:fail:unlike-assets:unresolved ([name any/c] [dependents list?])]{
An error raised when a resolver could not produce a @tech{resolved name}.

For a given instance @racket[E]:


@racket[(exn:fail:unlike-assets:unresolved-name E)] is a reference to the exact
name a user passed to a resolver.

@racket[(exn:fail:unlike-assets:cycle-dependents E)] is a
@tech{dependents list}. If the name resolution error did not occur,
the value named by the first element would be dependent on the resolved
variant of @racket[(exn:fail:unlike-assets:unresolved-name E)].
}

@defstruct*[exn:fail:unlike-assets:cycle ([scope procedure?] [dependency any/c] [dependents any/c])]{
An error raised when a @tech{resolver} or a thunk it produced
encounted a circular dependency.

For a given instance @racket[E]:

@racket[(exn:fail:unlike-assets:cycle-scope E)] is a procedure that would
never terminate if it weren't for this exception.

@racket[(exn:fail:unlike-assets:cycle-dependency E)] is a @tech{resolved name} of a
resource that is already a dependent, and therefore cannot be
resolved.

@racket[(exn:fail:unlike-assets:cycle-dependents E)] is a @tech{dependents list},
where the value named by the first element is dependent on the value named
by @racket[(exn:fail:unlike-assets:cycle-dependency E)].
}


@defproc[(raise-name-resolution-error [name any/c] [dependents list?]) any]{
Raises @racket[exn:fail:unlike-assets:unresolved] with the given
@racket[name], a preformatted message, and @racket[(current-continuation-marks)].
}


@defproc[(raise-cycle-error [scope procedure?] [name any/c] [dependents list?]) any]{
Raises @racket[exn:fail:unlike-assets:cycle] with the given arguments,
a preformatted message, and @racket[(current-continuation-marks)].
}
