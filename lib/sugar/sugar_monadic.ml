open Sugar_result

module type S = sig
  include Error

  type 'a monad
  type 'a result = ('a, error) generic_result monad

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

module Make (UserError2:Sugar_s.Error) (Monad:Sugar_s.Monad) : S
  with type error := UserError2.error
    and type 'a monad = 'a Monad.m
    and type 'a result = ('a, UserError2.error) generic_result Monad.m =
struct
  type 'a monad = 'a Monad.m
  type 'a result = ('a, UserError2.error) generic_result monad

  open Monad

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

  (* module Monad : Sugar_s.Monad
    with type 'a m := 'a monad =
  struct
    let return = commit
    let (>>=) = bind_if
  end *)
end
