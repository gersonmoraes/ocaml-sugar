open Types

(**
  A parametric module that implements the blocking interface.

  The complete documentation can be found in {!Types.Result}.
*)
module Make (UserError:Error) : Types.Result
  with type error = UserError.t =
struct
  type 'a result = ('a, UserError.t) Pervasives.result
  type error = UserError.t

  let return v = Ok v

  let throw e = Error e

  let bind_if r f =
    match r with
      | Error e -> Error e
      | Ok v -> f v

  let bind_unless r f =
    match r with
    | Error e -> f e
    | Ok v -> Ok v

  let map r f =
    match r with
    | Error e -> Error e
    | Ok v -> Ok (f v)


  module Infix = struct
    let (>>=) = bind_if
    let (>>|) = map
    let (>>>) x y =
      match x, y with
      | (Error e, _) -> Error e
      | _ -> y

    let (>---------) = bind_unless

    let (<*>) f x =
      f
      >>= fun f' ->
      x
      >>= fun x' ->
      return (f' x')

    let (<$>) f x = map x f
  end

  let unwrap = function
    | Ok r -> r
    | Error _ -> invalid_arg "Could not unwrap value from result"

  let unwrap_or f r =
    match r with
    | Ok r -> r
    | Error e -> f e

  let expect r msg =
    match r with
    | Ok r -> r
    | Error _ -> invalid_arg msg


  let (>>=) = bind_if

  let (>>) x y =
    x
    >>= fun () ->
    y

  module Monad : Types.Monad
    with type 'a t = 'a result =
  struct
    type 'a t = 'a result

    let return = return
    let (>>=) = bind_if
  end

  module For(M: Types.Monad) = struct
    include Promise.Make (M) (UserError)
  end
end