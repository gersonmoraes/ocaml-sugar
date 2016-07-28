open Sugar_types

type error = unit
type 'a result = 'a option

let commit v = Some v
let throw () = None

let bind_if r f =
  match r with
  | None -> None
  | Some v -> f v

(* This is an experimental function.
 * We still don't know if the second parameterer should be a thunk.
 *)
let bind_unless r f =
  match r with
  | None -> f ()
  | Some v -> Some v

let map r f =
  match r with
  | None -> None
  | Some v -> Some (f v)

(* Notice we are using these operators only with the option type.
 *
 * This means applications could use both types within a same module context.
 * This allows developers to use different result types between different
 * error handling layers.
 *)
let (&&=) = bind_if
let (||=) = bind_unless
let (&&|) = map

let (/>) = bind_if
let (//>) x y =
  match x, y with
  | None, _ -> None
  | _ -> y

(* This module implements a monadic interface for the option type
 * Notice though, that is is composed with aliases for other functions.
 *)
module Monad : Sugar_types.Monad
   with type 'a monad = 'a option =
struct
  type 'a monad = 'a option
  let return = commit
  let (>>=) = bind_if
end
