
 (* This should be refactored to the default result in OCaml >= 4.03  *)
 type ('a, 'b) std_result =
   | Ok of 'a
   | Error of 'b


 (** Common monadic signature *)
module type Monad = sig
  type 'a monad

  val return: 'a -> 'a monad
  val (>>=): 'a monad -> ('a -> 'b monad) -> 'b monad
end

(** Minimalistic Error interface *)
module type Error = sig
  type error
end

module type Result = sig
  include Error

  type 'a result
  (*type 'a result = ('a, error) std_result*)

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
