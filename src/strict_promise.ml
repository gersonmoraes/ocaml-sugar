open Abstract


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
module type S = sig

  (** Error definition imported from your project *)
  type error

  (** Evaluated result *)
  type 'a value = ('a, error) Result.result

  (**
    This type will be translated to the main
    monad in your project
   *)
  type 'a monad


  (**
    High level value type, created to simplify type hinting.
    It hides two things: your choice of asynchronous library and the relation
    with your project's error definition.

    For example, considere this function:
    {[
      let run () : unit result =
        return ()
    ]}

    The actual could be something like:
    {[
      (unit, error) Result.result Lwt.t
    ]}
  *)
  type 'a result = 'a value monad


  (**
     Similar to {{!Sugar__Sugar_result.S.bind} Sugar.Result.S.bind}
   *)
  val bind:  'a result -> ('a -> 'b result) -> 'b result


  (**
     Similar to {{!Sugar__Sugar_result.S.bind_unless} Sugar.Result.S.bind_unless}
   *)
  val bind_unless: 'a result -> (error -> 'a result) -> 'a result


 (**
    Similar to {{!Sugar__Sugar_result.S.map} Sugar.Result.S.map}
  *)
  val map:  'a result -> ('a -> 'b) -> 'b result


  (**
     Similar to {{!Sugar__Sugar_result.S.return} Sugar.Result.S.return}
  *)
  val return: 'a -> 'a result


  (**
    Similar to {{!Sugar__Sugar_result.S.throw} Sugar.Result.S.throw}
  *)
  val throw: error -> 'a result

  module Infix : sig

  (**
    Similar to {{!Sugar__Sugar_result.S.(>>|)} Sugar.Result.S.(>>|)}
  *)
  val (>>|): 'a result -> ('a -> 'b) -> 'b result

  (** Applicative combinator for map *)
  val (<$>): ('a -> 'b) -> 'a result -> 'b result

  (** Applicative combinator for parallel execution of function and operand *)
  val (<*>): ('a -> 'b) result -> 'a result -> 'b result

  (** An alias for UserMonad.(>>=)

      This combinator provides direct interaction with the underlying monad. *)
  val (>>>=): 'a monad -> ('a -> 'b monad) -> 'b monad

  val (>>>): 'a result -> 'b result Lazy.t -> 'b result
  val (>>): unit result -> 'a result Lazy.t -> 'a result

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
  (* val ( >> ) : unit result -> 'b result -> 'b result *)


  (*
    Ignore operator.

    Use this operator to ignore the previous value
    and return the next instruction.
  *)
  (* val (>>>): 'a result -> 'b result -> 'b result *)

  end

  (**
    Unwraps the successful value as a normal value in the threading monad.
    If the value is not successful, it will raise an Invalid_arg exception.
  *)
  val unwrap: 'a value monad -> 'a monad


  (**
    Unwraps the successful value as a value in the threading monad.
    Different from [unwrap], you can assign an error handler to be
    executed if the computation failed. Example:
    {[
    let run () =
      get_data ()
      |> unwrap_or (fun _ -> Lwt.return "default")
    ]}
  *)
  val unwrap_or: (error -> 'a monad) -> 'a value monad -> 'a monad


  (**
    Extracts a successful value from an computation, or raises and Invalid_arg
    exception with the defined parameter.
  *)
  val expect: 'a value monad -> string -> 'a monad

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
    Disable exception handling
  *)
  module NoExceptions : Promise.S
    with type error := error
    and type 'a monad := 'a monad
end


(**
  A parametric module that implements the monadic interface for values.
  The complete documentation can be found in {!Types.Promise}.
*)
module Make (UserError:StrictError) (UserMonad:StrictMonad) : S
  with
    type error := UserError.t
    and type 'a monad := 'a UserMonad.t
    and type 'a value = ('a, UserError.t) Result.result
    and type 'a result = ('a, UserError.t) Result.result UserMonad.t
=
struct
  include UserError

  type 'a monad = 'a UserMonad.t
  type 'a value = ('a, UserError.t) Result.result
  type 'a result = 'a value monad

  open UserMonad
  open Result

  let return v = UserMonad.return (Ok v)
  let throw e = UserMonad.return (Error e)

  let resolve r =
    UserMonad.catch
      ( fun () -> r )
      ( fun e -> throw (UserError.panic e) )

  let bind r f =
    resolve r
    >>= function
    | Error e -> throw e
    | Ok v -> f v

  let bind_unless r f =
    resolve r
    >>= function
    | Error e -> f e
    | Ok v -> return v

  let map r f =
    resolve r
    >>= function
    | Error e -> throw e
    | Ok v -> return (f v)

  module Infix = struct
    let (>>=) = bind

    let (>>|) = map

    let (>>) x y = bind x (fun () -> Lazy.force y)

    let (>>>) x y = bind x (fun _ -> Lazy.force y)

    let (>>>=) = UserMonad.(>>=)

    let (>---------) = bind_unless

    let (<*>) f x =
      f
      >>= fun f' ->
      x
      >>= fun x' ->
      return (f' x')

    let (<$>) f x = map x f
  end

  let unwrap r =
    resolve r
    >>= function
    | Ok v -> UserMonad.return v
    | Error e -> invalid_arg "Could not unwrap value"

  let unwrap_or f r =
    resolve r
    >>= function
    | Ok v -> UserMonad.return v
    | Error e -> f e

  let expect r msg =
    resolve r
    >>= function
    | Ok v -> UserMonad.return v
    | Error e -> invalid_arg msg

  let (>>=) = bind

  module NoExceptions = Promise.Make (UserError) (UserMonad)
end
