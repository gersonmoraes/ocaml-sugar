open Sugar_types

(**
  A functor that implements the asynchronous interface.
  The complete documentation can be found in {!Sugar_types.Promise}.
*)
module Make
  (UserMonad:Sugar_types.Monad) (NaturalError:Sugar_types.NaturalError)
: Sugar_types.NaturalPromise
  with
    type error_src = NaturalError.src
    and type error = NaturalError.dst
    and type 'a monad := 'a UserMonad.t
    and type 'a result = ('a, NaturalError.dst) Pervasives.result
    and type 'a promise = ('a, NaturalError.dst) result UserMonad.t
=
struct
  (* include UserError *)
  type error_src = NaturalError.src
  type error = NaturalError.dst

  type 'a monad = 'a UserMonad.t
  type 'a result = ('a, NaturalError.dst) Pervasives.result
  type 'a promise = 'a result monad

  open UserMonad

  let return v = UserMonad.return (Ok v)
  let throw (e:NaturalError.src) =
    UserMonad.return (Error (NaturalError.apply e))

  let throw_dst (e:NaturalError.dst) = UserMonad.return (Error e)

  let bind_if r f =
    r
    >>= function
    | Ok v -> f v
    | Error e -> throw_dst e

  let bind_unless r f =
    r
    >>= function
    | Ok v -> return v
    | Error e ->
      ( match NaturalError.reverse e with
        | Some v -> f v
        | None -> throw_dst e
      )

  let map r f =
    r
    >>= function
    | Ok v -> return (f v)
    | Error e -> throw_dst e

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

    let (<$>) f x = map x f

    let (<*>) f x =
      f
      >>= fun f' ->
      x
      >>= fun x' ->
      return (f' x')
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
    | Error e ->
      ( match NaturalError.reverse e with
        | Some v -> f v
        | None -> invalid_arg "Could not unwrap result"
      )

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
