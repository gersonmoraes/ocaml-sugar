open Sugar_types
(*
module type Promise_s = sig

  open Sugar_result

  include Sugar_types.Error
  include Sugar_types.Monad

  type 'a result = ('a, error) std_result

  (* = (('a , Error.error) std_result) Monad.m *)

  (**
   * Apply the binding only if the computation was successful.
   *
   * You can use the operator (&&=) instead of this function for syntatic sugar
   *)
  val bind_if:  'a result monad -> ('a -> 'b result monad) -> 'b result monad

  (**
   * Apply the binding only if the computation failed.
   *
   * Notice that an error handler must be provided, and this handler
   * must throw an error or provide an equivalent for the result type of the
   * previous computation.
   *
   * You can use the operator (||=) instead of this function for syntatic sugar
   *)
  val bind_unless: 'a result monad -> (error -> 'a result monad) -> 'a result monad

  (**
   * Apply a function to the wraped value if the result is successful
   *)
  val map:  'a result monad -> ('a -> 'b) -> 'b result monad

  (** Indicate a successful computation *)
  val commit: 'a -> 'a result monad

  (** Indicate a failure in a computation *)
  val throw: error -> 'a result monad

  (** Conditional binding operator AND *)
  val (&&=): 'a result monad -> ('a -> 'b result monad) -> 'b result monad

  (** Conditional binding operator OR *)
  val (||=): 'a result monad -> (error -> 'a result monad) -> 'a result monad
end
*)

open Sugar_result

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

  let return = UserMonad.return
  let (>>=) = UserMonad.(>>=)

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