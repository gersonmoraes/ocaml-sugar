open Types

(**
  An implementation of {{!Types.Result}  Sugar.Types.Result } interface
  for the option type.

  This is probably the easiest way to start using Sugar, as there is no need to
  use describe errors or use functors. Still, because this module follows the
  same API, when you need to start refactoring to more complex, non-blocking
  results you get to keep the same clean API, making be transition
  straightfoward.

  Usage example:
  <code>
    open Sugar.option

    let some_computation (): string result =
      if true then
        Some "you could use any option type"
      else
        throw ()

    let run (): string result =
      some_computation ()
      ||= fun () ->
      commit "recovered"
  </code>

  In case you are wondering, the evaluation of [run ()] in the example above,
  will produce: [string option = Some "you could use any option type"].
*)

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

module Monad : Types.Monad
   with type 'a t = 'a option =
struct
  type 'a t = 'a option
  let return = commit
  let (>>=) = bind_if
end
