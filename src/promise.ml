open Abstract

(**
  This interface specifies an error handling layer for monadic computations.

  Sugar result modules work with any monad.

  <code>
    module MyMonad = struct
      type 'a monad = 'a Lwt.t
      let return = Lwt.return
      let (>>=) = Lwt.bind
    end
    module MyResult = Sugar.Promise.Make (MyMonad) (MyError)
  </code>
*)
module type S = sig

  (** Error definition imported from your project *)
  type error

  (** Low level result type *)
  type 'a result = ('a, error) Pervasives.result

  (**
    This is a virtual type that will be translated to your asynchronous
    library's main type.
   *)
  type 'a monad


  (**
    High level result type, created to simplify type hinting.
    It hides two things: your choice of asynchronous library and the relation
    with your project's error definition.

    For example, considere this function:
    <code>
      let run () : unit promise =
        return ()
    </code>

    The actual could be something like:
    <code>
      (unit, error) Pervasives.result Lwt.t
    </code>
  *)
  type 'a promise = 'a result monad


  (**
     Similar to {{!Result.bind_if} Result.bind_if}
   *)
  val bind_if:  'a promise -> ('a -> 'b promise) -> 'b promise


  (**
     Similar to {{!Result.bind_unless} Result.bind_unless}
   *)
  val bind_unless: 'a promise -> (error -> 'a promise) -> 'a promise


 (**
    Similar to {{!Result.map} Result.map}
  *)
  val map:  'a promise -> ('a -> 'b) -> 'b promise


  (**
     Similar to {{!Result.return} Result.return}
  *)
  val return: 'a -> 'a promise


  (**
    Similar to {{!Result.throw} Result.throw}
  *)
  val throw: error -> 'a promise

  module Infix : sig

  (**
    Similar to {{!Result.(>>|)} Result.(>>|)}
  *)
  val (>>|): 'a promise -> ('a -> 'b) -> 'b promise

  (** Applicative combinator for map *)
  val (<$>): ('a -> 'b) -> 'a promise -> 'b promise

  (** Applicative combinator for parallel execution of function and operand *)
  val (<*>): ('a -> 'b) promise -> 'a promise -> 'b promise

  (**
    Broom combinator

    Used to introduce an error handler block to "clean errors".

    There's a secret message behind the form of this combinator.
    It has the same number of characters sufficient for the whole block
    in the next line. For example:

    <code>
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
    </code>

    So beyond the clean aesthetics similar to markdown, we are
    implying that a developer should never handle errors in an open
    anonymous function.
  *)
  val (>---------): 'a promise -> (error -> 'a promise) -> 'a promise


  (**
    Ignore operator.

    Use this operator to ignore the previous result
    and return the next instruction.
  *)
  val (>>>): 'a promise -> 'b promise -> 'b promise

  end

  (**
    Unwraps the successful result as a normal value in the threading monad.
    If the value is not successful, it will raise an Invalid_arg exception.
  *)
  val unwrap: 'a result monad -> 'a monad


  (**
    Unwraps the successful result as a value in the threading monad.
    Different from [unwrap], you can assign an error handler to be
    executed if the computation failed. Example:
    <code>
    let run () =
      get_data ()
      |> unwrap_or (fun _ -> Lwt.return "default")
    </code>
  *)
  val unwrap_or: (error -> 'a monad) -> 'a result monad -> 'a monad


  (**
    Extracts a successful value from an computation, or raises and Invalid_arg
    exception with the defined parameter.
  *)
  val expect: 'a result monad -> string -> 'a monad

  (**
    Bind combinator

    If the computation in the left is successful, the operator will
    Take the inner value and feed it to the function in the right. This is an
    alias for the function [bind_if].

    If the computation in the left failed, the operator will propagate the error,
    skipping the function completely.
  *)
  val (>>=): 'a promise -> ('a -> 'b promise) -> 'b promise


 (**
   Semicolon combinator.

   Like the standard semicolon in OCaml, ";", the previous operation needs
   to evaluate to a unit promise.
 *)
 val ( >> ) : unit promise -> 'b promise -> 'b promise

end


(**
  A parametric module that implements the monadic interface for results.
  The complete documentation can be found in {!Types.Promise}.
*)
module Make  (UserMonad:Monad)  (UserError:Error) : S
  with
    type error := UserError.t
    and type 'a monad := 'a UserMonad.t
    and type 'a result = ('a, UserError.t) Pervasives.result
    and type 'a promise = ('a, UserError.t) result UserMonad.t
=
struct
  include UserError

  type 'a monad = 'a UserMonad.t
  type 'a result = ('a, UserError.t) Pervasives.result
  type 'a promise = 'a result monad

  open UserMonad

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
    (* let (&&=) = bind_if
    let (||=) = bind_unless
    let (&&|) = map *)

    let (>>|) = map

    let (>>>) x y =
      x
      >>= fun _ ->
      y

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
    | Error e -> invalid_arg "Could not unwrap result"

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
  let (>>) x y =
    x
    >>= fun () ->
    y
end
