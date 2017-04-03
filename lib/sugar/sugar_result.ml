open Sugar_types

(**
  A functor that implements the blocking interface.

  This functors produces a module following the interface {!Sugar_types.Result}.
*)
module Make (UserError:Error) : Sugar_types.Result
  with type error := UserError.error =
struct
  type 'a result = ('a, UserError.error) Pervasives.result

  let commit v = Ok v

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

    let (&&=) = bind_if

    let (||=) = bind_unless

    let (&&|) = map

    let (/>) = bind_if

    let (//>) x y =
      match x, y with
      | (Error e, _) -> Error e
      | _ -> y

    let (>>) x y = y

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

  module Monad : Sugar_types.Monad
    with type 'a monad = 'a result =
  struct
    type 'a monad = 'a result

    let return = commit
    let (>>=) = bind_if
  end
end
