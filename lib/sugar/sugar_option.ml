open Sugar_types

type error = unit
type 'a result = 'a option

let commit v = Some v
let throw () = None

let bind_if r f =
  match r with
  | None -> None
  | Some v -> f v

let bind_unless r f =
  match r with
  | None -> f ()
  | Some v -> Some v

let map r f =
  match r with
  | None -> None
  | Some v -> Some (f v)

let (&&=) = bind_if
let (||=) = bind_unless
let (&&|) = map

let (/>) = bind_if
let (//>) x y =
  match x, y with
  | None, _ -> None
  | _ -> y

let wrap f =
  try Some (f ()) with
  | e -> None

let unwrap = function
  | Some r -> r
  | None -> invalid_arg "Could not unwrap value from result"

let unwrap_or r f =
  match r with
  | Some r -> r
  | None -> f ()

let expect r msg =
  match r with
  | Some r -> r
  | None -> invalid_arg msg

(**
  This module implements a monadic interface for the option type
  Notice though, that is is composed with aliases for other functions.
 *)
module Monad : Sugar_types.Monad
   with type 'a monad = 'a option =
struct
  type 'a monad = 'a option
  let return = commit
  let (>>=) = bind_if
end
