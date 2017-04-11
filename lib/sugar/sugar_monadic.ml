open Sugar_types

(**
  A functor that implements the asynchronous interface.
  The complete documentation can be found in {!Sugar_types.Promise}.
*)
module Make  (UserMonad:Sugar_types.Monad)  (UserError:Sugar_types.Error) : Sugar_types.Promise
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
