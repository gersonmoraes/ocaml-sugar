(**
  This module provides signatures for the dependencies used in Sugar's module builders.
  Users do not need to access these interfaces directly.
 *)

(**
  A generic signature describing a monad.
*)
module type Monad = sig

  type 'a t
  (** A parametric type representing any OCaml value. *)

  val return: 'a -> 'a t
  (** Creates a constant value in this monad.  *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Waits for the conclusion of the monad in the left,
      and then, apply the unwrapped value to the function in the right.
   *)

end


(**
  Conventional module type to define the errors inside a project.
  This is how Sugar understands the error handling layer of a project.

  Like:
  {[
    module MyError = struct
      type error = Not_found | Invalid_arg of string
    end
  ]}

  This module might be used to create blocking or asynchronous error handling
  layers, using the Sugar functors, like:
  {[
    module MyResult = Sugar.Result.Make (MyError)

    module MyResult2 = Sugar.Promise.Make (Lwt) (MyError)
    module MyResult2 = MyResult.For (Lwt)
  ]}
*)
module type Error = sig

  type t
  (**
    This type describes the errors of your project. It's one of the main requirements
    to create a result monad.

    If you don't want to specify your errors upfront, you can still use something like [unit] or
    [string] as error type.
  *)

end

(**
  This signature describes an [Error] module that has some control over unexpected exceptions.

  If you want to handle unexpected exceptions as they appear, you should probably define
  an error case with the type [exn], like in the code below:
  {[
  module Error = struct
    type t =
      | Because_reasons
      | Unexpected of exn

    let panic e = Unexpected e
  end
  ]}
 *)
module type StrictError = sig
  include Error

  (**
    When an exception is detected, this module can either terminate the process with a proper
    message or chose convert the error to the type {!t}.
  *)
  val panic : exn -> t
end

(**
  A monad that provides some awareness about unexpected exceptions.

  This module is related to {{!Sugar.Abstract.StrictError} StrictError}.
*)
module type StrictMonad = sig
  include Monad

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  (**
    Checks if the monad returned by the thunk raised an exception, and applies
    the given error handler if necessary.

    This function has intentionally the
    exact signature as [Lwt.catch]. This means the [Lwt] module is already a [StrictMonad]:
    {[
    let _ =
      (module Lwt: Sugar.Abstract.StrictMonad)
    ]}
   *)
end
