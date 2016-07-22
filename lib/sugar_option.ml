open Sugar_std

let commit v = Some v

let bind r f =
  match r with
  | None -> None
  | Some v -> f v

let map r f =
  match r with
  | None -> None
  | Some v -> Some (f v)

(* This is an experimental function.
 * We still don't know if the second parameterer should be a thunk.
 *)
let catch r x =
  match r with
  | None -> x
  | Some v -> Some v

(* Notice we are using these operators only with the option type.
 *
 * This means applications could use both types within a same module context.
 * This allows developers to use different result types between different
 * error handling layers.
 *)
let (&=) = bind
let (|=) = catch

(* This module implements a monadic interface for the option type
 * Notice though, that is is composed with aliases for other functions.
 *)
  module Monad : Monad
   with type 'a m := 'a option =
 struct
   let return = commit
   let (>>=) = bind
 end
