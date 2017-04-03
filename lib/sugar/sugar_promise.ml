open Sugar_types

(**
  A functor that implements the asynchronous interface.
  The complete documentation can be found in {!Sugar_types.Promise}.
*)
module Make  (UserMonad:Sugar_types.Monad)  (UserError:Sugar_types.Error) : Sugar_types.Promise
  with
    type error := UserError.error
    and type 'a monad := 'a UserMonad.monad
    and type 'a result = ('a, UserError.error) Pervasives.result
    and type 'a promise = ('a, UserError.error) result UserMonad.monad
=
struct
  include UserError

  type 'a monad = 'a UserMonad.monad
  type 'a result = ('a, UserError.error) Pervasives.result
  type 'a promise = 'a result monad

  open UserMonad

  let commit v = return (Ok v)
  let throw e = return (Error e)

  let bind_if r f =
    r
    >>= function
    | Error e -> throw e
    | Ok v -> f v

  let bind_unless r f =
    r
    >>= function
    | Error e -> f e
    | Ok v -> commit v

  let map r f =
    r
    >>= function
    | Error e -> throw e
    | Ok v -> commit (f v)

  module Infix = struct
    let (>>=) = bind_if
    let (&&=) = bind_if
    let (||=) = bind_unless
    let (&&|) = map

    let (/>) x y = bind_if x y
    let (>>) x y = x &&= fun _ -> y
  end

  let unwrap r =
    r
    >>= function
    | Ok v -> return v
    | Error e -> invalid_arg "Could not unwrap result"

  let unwrap_or f r =
    r
    >>= function
    | Ok v -> return v
    | Error e -> f e

  let expect r msg =
    r
    >>= function
    | Ok v -> return v
    | Error e -> invalid_arg msg

  let (>>=) = bind_if
end
