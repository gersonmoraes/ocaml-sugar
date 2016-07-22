open Sugar_std

(**
 * Difine a common result type. This definition is experimental.
 *
 * A functor for this types might be created.
 * When we adapt this module for OCaml 4.03, we'll use Core.Std.result type
 *)
(* module Std = struct
  type ('a, 'b) result = Ok of 'a | Error of 'b
end *)


module type S = sig
  include Sugar_std.Error

  type 'a result = ('a, error) std_result

  (**
   * Apply the binding only if the computation was successful.
   *
   * You can use the operator (&&=) instead of this function for syntatic sugar
   *)
  val bind_if:  'a result -> ('a -> 'b result) -> 'b result

  (**
   * Apply the binding only if the computation failed.
   *
   * Notice that an error handler must be provided, and this handler
   * must throw an error or provide an equivalent for the result type of the
   * previous computation.
   *
   * You can use the operator (||=) instead of this function for syntatic sugar
   *)
  val bind_unless: 'a result -> (error -> 'a result) -> 'a result

  (**
   * Apply a function to the wraped value if the result is successful
   *)
  val map:  'a result -> ('a -> 'b) -> 'b result

  (** Indicate a successful computation *)
  val commit: 'a -> 'a result

  (** Indicate a failure in a computation *)
  val throw: error -> 'a result

  (** Conditional binding operator AND *)
  val (&&=): 'a result -> ('a -> 'b result) -> 'b result

  (** Conditional binding operator OR *)
  val (||=): 'a result -> (error -> 'a result) -> 'a result
end


module Make (UserError:Error) : S
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

  module Monad : Sugar_std.Monad
    with type 'a monad := 'a result =
  struct
    let return = commit
    let (>>=) = bind_if
  end
end
