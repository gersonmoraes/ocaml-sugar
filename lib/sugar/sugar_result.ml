open Sugar_types

(**
 * Difine a common result type. This definition is experimental.
 *
 * A functor for this types might be created.
 * When we adapt this module for OCaml 4.03, we'll use Core.Std.result type
 *)
(* module Std = struct
  type ('a, 'b) result = Ok of 'a | Error of 'b
end *)


module Make (UserError:Error) : Sugar_types.Result
  with type error := UserError.error =
struct
  type 'a result = ('a, UserError.error) std_result

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

  let (&&=) = bind_if
  let (||=) = bind_unless

  module Monad : Sugar_types.Monad
    with type 'a monad = 'a result =
  struct
    type 'a monad = 'a result
    
    let return = commit
    let (>>=) = bind_if
  end
end
