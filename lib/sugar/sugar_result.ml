open Sugar_types

(**
  A functor that implements the blocking interface.

  This functors produces a module following the interface {!Sugar_types.Result}.
*)
module Make (UserError:Error) : Sugar_types.Result
  with type error = UserError.error =
struct
  type 'a result = ('a, UserError.error) Pervasives.result
  type error = UserError.error

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
    let (>>) x y =
      match x, y with
      | (Error e, _) -> Error e
      | _ -> y

    let (>---------) = bind_unless
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

  let (/>) x y =
    x
    >>= fun () ->
    y

  module Monad : Sugar_types.Monad
    with type 'a t = 'a result =
  struct
    type 'a t = 'a result

    let return = return
    let (>>=) = bind_if
  end

  module For(M: Sugar_types.Monad) = struct
    include Sugar_promise.Make (M) (UserError)
  end
end
