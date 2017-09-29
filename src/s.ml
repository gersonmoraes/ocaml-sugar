(**
  Sugar module signatures.
 *)


(**
  Signatures for the dependencies used in Sugar's module builders.
*)
module Params = struct


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
module type Strict_error = sig
  include Error

  (**
    When an exception is detected, this module can either terminate the process with a proper
    message or chose convert the error to the type {!t}.
  *)
  val panic : exn -> t
end


(**
  A monad that provides some awareness about unexpected exceptions.

  This module is related to {{!Sugar.Params.Strict_error} Strict_error}.
*)
module type Strict_monad = sig
  include Monad

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  (**
    Checks if the monad returned by the thunk raised an exception, and applies
    the given error handler if necessary.

    This function has intentionally the
    exact signature as [Lwt.catch]. This means the [Lwt] module is already a [Strict_monad]:
    {[
    let _ =
      (module Lwt: Sugar.Params.Strict_monad)
    ]}
   *)
end


end

open Params



module type Promise = sig

  (** Error definition imported from your project *)
  type error

  (** An alias for [Pervasives.result] that can only work with errors of your project. *)
  type 'a value = ('a, error) Result.result

  (**
    This type is an alias to the underlining monad used to create your [Promise] module.
    In other words, it points to the main type of your threading library ([Lwt.t] for Lwt,
    or [Async.Std.Deferred.t] for Async).
   *)
  type 'a monad


  (**
    This type describes a result monad inside a project.

    For example, if the concrete module is built on top of Lwt,
    a value of type [unit result] will be translated as [(unit, error) Pervasives.result Lwt.t].
  *)
  type 'a result = 'a value monad


  (**
    This combinator is also called [bind].

    It can be used to chain sequential operations with the current monad.
    If the computation in the left failed, the operator will propagate the error,
    skipping the function completely.
  *)
  val (>>=): 'a result -> ('a -> 'b result) -> 'b result


  (**
     Similar to {{!Sugar__S.Result.bind} S.Result.bind}
   *)
  val bind:  'a result -> ('a -> 'b result) -> 'b result


  (**
     Similar to {{!Sugar__S.Result.bind_unless} S.Result.bind_unless}
   *)
  val bind_unless: 'a result -> (error -> 'a result) -> 'a result


 (**
    Similar to {{!Sugar__S.Result.map} S.Result.map}
  *)
  val map:  'a result -> ('a -> 'b) -> 'b result


  (**
     Similar to {{!Sugar__S.Result.return} S.Result.return}
  *)
  val return: 'a -> 'a result


  (**
    Similar to {{!Sugar__S.Result.throw} S.Result.throw}
  *)
  val throw: error -> 'a result

  module Infix : sig

  (**
    Similar to {{!Sugar__S.Result.(>>|)} S.Result.(>>|)}
  *)
  val (>>|): 'a result -> ('a -> 'b) -> 'b result

  (** Applicative combinator for map *)
  val (<$>): ('a -> 'b) -> 'a result -> 'b result

  (** Applicative combinator for parallel execution of function and operand *)
  val (<*>): ('a -> 'b) result -> 'a result -> 'b result

  (** An alias for UserMonad.(>>=)

      This combinator provides direct interaction with the underlying monad. *)
  val (>>>=): 'a monad -> ('a -> 'b monad) -> 'b monad

  (**
    Broom combinator

    Used to introduce an error handler block to "clean errors".

    There's a secret message behind the form of this combinator.
    It has the same number of characters sufficient for the whole block
    in the next line. For example:

    {[
    let program1 () =
      do_something ()
      >---------
      ( fun e ->
        return ()
      )

    let program2 () =
      do_something ()
      >---------
      ( function
        e -> return ()
      )
    ]}

    So beyond the clean aesthetics similar to markdown, we are
    implying that a developer should never handle errors in an open
    anonymous function.
  *)
  val (>---------): 'a result -> (error -> 'a result) -> 'a result


  (*
    Semicolon combinator.

    Like the standard semicolon in OCaml, ";", the previous operation needs
    to evaluate to a unit result.
  *)
  val ( >> ) : unit result -> 'b result Lazy.t -> 'b result


  (*
    Ignore operator.

    Use this operator to ignore the previous value
    and return the next instruction.
  *)
  val (>>>): 'a result -> 'b result Lazy.t -> 'b result

  end

  (**
    Unwraps the successful value as a normal value in the threading monad.
    If the value is not successful, it will raise an [Invalid_arg] exception.
  *)
  val unwrap: 'a value monad -> 'a monad


  (**
    Unwraps the successful value as a value in the threading monad.
    Different from [unwrap], you can assign an error handler to be
    executed if the computation failed.
  *)
  val unwrap_or: (error -> 'a monad) -> 'a value monad -> 'a monad


  (**
    Extracts a successful value from an computation, or raises and [Invalid_arg]
    exception with a customized error message.
  *)
  val expect: 'a value monad -> string -> 'a monad

end

(**
  This interface specifies an error handling layer for monadic computations.

  Sugar value modules work with any monad.

  {[
    module MyMonad = struct
      type 'a monad = 'a Lwt.t
      let return = Lwt.return
      let (>>=) = Lwt.bind
    end
    module MyResult = Sugar.Promise.Make (MyMonad) (MyError)
  ]}
*)
module type Strict_promise = sig
  include Promise

  (**
    Disable exception handling. Open this module with you don't want to catch exceptions.
  *)
  module NoExceptions : Promise
    with type error := error
    and type 'a monad := 'a monad

end












(**
  The signature for the default result monad.
*)
module type Result_partials = sig

  (**
    Error definition from your project
  *)
  type error


  (**
    An alias for the result type in the stdlib
  *)
  type 'a result = ('a, error) Result.result


  (**
    Apply the binding only if the computation was successful.
    You can use the operator {{!(>>=)} >>=} instead of this function for syntatic sugar
   *)
  val bind:  'a result -> ('a -> 'b result) -> 'b result


  (**
    Apply the binding only if the computation failed.

    Notice that an error handler must be provided, and this handler
    must throw an error or provide an equivalent for the result type of the
    previous computation.

    You can use the operator {{!Infix.(>---------)} >---------} instead of this function for syntatic sugar
   *)
  val bind_unless: 'a result -> (error -> 'a result) -> 'a result


  (**
    Apply a function to the result of a successful computation. This function
    makes it ease to work with non error aware functions.

    Example:
    {[
    open Sugar.Option

    let twenty =
     map (Some 10) (fun n -> n + n)
    ]}

    You could also use the combinator {{!Infix.(>>|)} >>|} for syntatic sugar.
   *)
  val map:  'a result -> ('a -> 'b) -> 'b result

  (**
    Return a value in a successful computation.

    This function should be used with its counterpart, [throw]
  *)
  val return: 'a -> 'a result


  (**
    Return an error as the result of a computation.

    Like the [return] function, [throw] helps you hide the internals of your
    result type and keep a clean code.

    If you are still at the beginning of your project, and don't have your
    errors defined yet, this function still is a great help. For example,
    the code bellow have the same usage as the function [failwith], but is a lot
    safer.

    {[
      module MyResult = Sugar.MakeResult (struct error = string end)
      open MyResult
      let run (): int result =
        if true then
          return 10
        else
          throw "something bad happend"
    ]}

    You could also not describe your errors at all for some time, and
    use the {!Sugar.Option} module to create error aware computations, like:

    {[
      open Sugar.Option
      let run (): string result =
        if true then
          return "hello world"
        else
          throw ()
    ]}

   *)
  val throw: error -> 'a result

  module Infix : sig


    (**
      Broom combinator

      Used to introduce an error handler block to "clean errors".

      There's a secret message behind the form of this combinator.
      It has the same number of characters sufficient for the whole block
      in the next line. For example:

      {[
      let program1 () =
        do_something ()
        >---------
        ( fun e ->
          return ()
        )

      let program2 () =
        do_something ()
        >---------
        ( function
          e -> return ()
        )
      ]}

      So beyond the clean aesthetics similar to markdown, we are
      implying that a developer should never handle errors in an open
      anonymous function.
    *)
    val (>---------): 'a result -> (error -> 'a result) -> 'a result


    (**
      Combinator for map with semantic similar to bind

      As its name sugests, this is an alias for the function {{!map} map}.
      Its intended use is to help integrate with functions that are not error
      aware.

      For example, considere the function [let double x = x + x] in the code
      fragments bellow:

      {[
       open Sugar.Option

       let twenty =
         match Some 10 with
         | None -> None
         | Some n -> Some (double n)

        let using_bind_combinator =
         Some 10
         >>=
         ( fun n ->
           return (double n)
         )

       let using_map_combinator =
         Some 10
         >>| double
      ]}
    *)
    val (>>|): 'a result -> ('a -> 'b) -> 'b result


    (*
      Ignore operator.

      Use this operator to ignore the previous result
      and return the next instruction.
    *)
    (* val (>>>): 'a result -> 'b result -> 'b result *)

    (* val (>>=): 'a result -> ('a -> 'b result) -> 'b result *)


    val (<$>): ('a -> 'b) -> 'a result -> 'b result
    val (<*>): ('a -> 'b) result -> 'a result -> 'b result


    (**
      Ignore operator.

      Use this operator to ignore the previous result
      and return the next instruction.
    *)
    val (>>>): 'a result -> 'b result Lazy.t -> 'b result

    val (>>): unit result -> 'b result Lazy.t -> 'b result

  end


  (**
    Bind combinator

    If the computation in the left is successful, the operator will
    Take the inner value and feed it to the function in the right. This is an
    alias for the function [bind].

    If the computation in the left failed, the operator will propagate the error,
    skipping the function completely.
  *)
  val (>>=): 'a result -> ('a -> 'b result) -> 'b result

  (**
    Unwraps the successful result as a normal value in the threading monad.
    If the value is not successful, it will raise an Invalid_arg exception.
  *)
  val unwrap: 'a result -> 'a


  (**
    Unwraps the successful result as a value in the threading monad.
    Different from [unwrap], you can assign an error handler to be
    executed if the computation failed. Example:
    {[
    let run () =
      get_data ()
      |> unwrap_or (fun _ -> "default")
    ]}
  *)
  val unwrap_or: (error -> 'a) -> 'a result -> 'a


  (**
    Extracts a successful value from an computation, or raises and Invalid_arg
    exception with the defined parameter.
  *)
  val expect: 'a result -> string -> 'a

end



module type Result = sig
  include Result_partials

  (**
    Create a new result module based on the current one, but wrapped around a monad.
  *)
  module For : functor (UserMonad:Monad) -> Promise
    with type error := error
    and type 'a monad := 'a UserMonad.t
end



module type Strict_result = sig
  include Result_partials

  (**
    Create a new result module based on the current one, but wrapped around a monad.
  *)
  module For : functor (UserMonad:Params.Strict_monad) -> Strict_promise
    with type error := error
    and type 'a monad := 'a UserMonad.t


  (**
    Disable exception handling
  *)
  module NoExceptions : Result
    with type error := error
end















(*  *)
