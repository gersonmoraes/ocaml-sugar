open Abstract

(**
  This interface specifies an error handling layer for monadic computations.

  Sugar score modules work with any monad.

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
  type 'a score = ('a, error) Result.result

  (**
    This type will be translated to the main
    monad in your project
   *)
  type 'a monad


  (**
    High level score type, created to simplify type hinting.
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
  type 'a result = 'a score monad


  (**
     Similar to {{!Sugar_result.S.bind_if} Result.bind_if}
   *)
  val bind_if:  'a result -> ('a -> 'b result) -> 'b result


  (**
     Similar to {{!Result.bind_unless} Result.bind_unless}
   *)
  val bind_unless: 'a result -> (error -> 'a result) -> 'a result


 (**
    Similar to {{!Result.map} Result.map}
  *)
  val map:  'a result -> ('a -> 'b) -> 'b result


  (**
     Similar to {{!Result.return} Result.return}
  *)
  val return: 'a -> 'a result


  (**
    Similar to {{!Result.throw} Result.throw}
  *)
  val throw: error -> 'a result

  module Infix : sig

  (**
    Similar to {{!Result.(>>|)} Result.(>>|)}
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

    Use this operator to ignore the previous score
    and return the next instruction.
  *)
  val (>>>): 'a result -> 'b result Lazy.t -> 'b result

  end

  (**
    Unwraps the successful score as a normal value in the threading monad.
    If the value is not successful, it will raise an Invalid_arg exception.
  *)
  val unwrap: 'a score monad -> 'a monad


  (**
    Unwraps the successful score as a value in the threading monad.
    Different from [unwrap], you can assign an error handler to be
    executed if the computation failed. Example:
    {[
    let run () =
      get_data ()
      |> unwrap_or (fun _ -> Lwt.return "default")
    ]}
  *)
  val unwrap_or: (error -> 'a monad) -> 'a score monad -> 'a monad


  (**
    Extracts a successful value from an computation, or raises and Invalid_arg
    exception with the defined parameter.
  *)
  val expect: 'a score monad -> string -> 'a monad

  (**
    Bind combinator

    If the computation in the left is successful, the operator will
    Take the inner value and feed it to the function in the right. This is an
    alias for the function [bind_if].

    If the computation in the left failed, the operator will propagate the error,
    skipping the function completely.
  *)
  val (>>=): 'a result -> ('a -> 'b result) -> 'b result

end


(**
  A parametric module that implements the monadic interface for scores.
  The complete documentation can be found in {!Types.Promise}.
*)
module Make (UserError:Error) (UserMonad:Monad) : S
  with
    type error := UserError.t
    and type 'a monad := 'a UserMonad.t
    and type 'a score = ('a, UserError.t) Result.result
    and type 'a result = ('a, UserError.t) Result.result UserMonad.t
=
struct
  include UserError

  type 'a monad = 'a UserMonad.t
  type 'a score = ('a, UserError.t) Result.result
  type 'a result = 'a score monad

  open UserMonad
  open Result

  let return v = UserMonad.return (Ok v)
  let throw e = UserMonad.return (Error e)

  let bind_if r f =
    r
    >>= function
    | Error e -> throw e
    | Ok v -> f v

  let bind_unless r f =
    r
    >>= function
    | Error e -> f e
    | Ok v -> return v

  let map r f =
    r
    >>= function
    | Error e -> throw e
    | Ok v -> return (f v)

  module Infix = struct
    let (>>=) = bind_if

    let (>>|) = map

    let (>>) x y = bind_if x (fun () -> Lazy.force y)

    let (>>>) x y = bind_if x (fun _ -> Lazy.force y)

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
    r
    >>= function
    | Ok v -> UserMonad.return v
    | Error e -> invalid_arg "Could not unwrap score"

  let unwrap_or f r =
    r
    >>= function
    | Ok v -> UserMonad.return v
    | Error e -> f e

  let expect r msg =
    r
    >>= function
    | Ok v -> UserMonad.return v
    | Error e -> invalid_arg msg


  let (>>=) = bind_if
end
