open Sugar_types

module Make  (UserMonad:Sugar_types.Monad)  (UserError:Sugar_types.Error) : Sugar_types.Promise
  with
    type error := UserError.error
    and type 'a promise := 'a UserMonad.monad
    (* and type 'a result = ('a, UserError.error) std_result *)
    and type 'a state = ('a, UserError.error) std_result
    and type 'a result = (('a, UserError.error) std_result) UserMonad.monad
=
struct
  (* type error = UserError.error *)
  include UserError
  type 'a promise = 'a UserMonad.monad

  type 'a state = ('a, error) std_result
  type 'a result = 'a state promise

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

  let (&&=) = bind_if
  let (||=) = bind_unless
  let (&&>) x y  = semicolon x y

  module Monad : Sugar_types.Monad
    with type 'a monad = 'a result =
  struct
    type 'a monad = 'a result
    let return = commit
    let (>>=) = bind_if
    let semicolon = (&&>)
  end
end
