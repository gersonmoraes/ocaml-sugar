(**
 * This module defines minimalistic interfaces for all Sugar modules
 *)

type ('a, 'b) std_result =
  | Ok of 'a
  | Error of 'b
 (** This should be refactored to the default result in OCaml >= 4.03  *)


 (** Common monadic signature *)
module type Monad = sig
  type 'a monad
  val return: 'a -> 'a monad
  val (>>=): 'a monad -> ('a -> 'b monad) -> 'b monad

  (* val semicolon: 'a monad -> 'b monad -> 'b monad *)
end

(** Minimalistic Error interface *)
module type Error = sig
  type error
  (**
   * Type for errors inside a project.
   *
   * Use covariant typesd if you want to split your error definitions between
   * different specialized modules.
   *)
end

module type Result = sig
  include Error

  type 'a result
  (*type 'a result = ('a, error) std_result*)

  val bind_if:  'a result -> ('a -> 'b result) -> 'b result
  (**
   * Apply the binding only if the computation was successful.
   * You can use the operator (&&=) instead of this function for syntatic sugar
   *)

  val bind_unless: 'a result -> (error -> 'a result) -> 'a result
  (**
   * Apply the binding only if the computation failed.
   *
   * Notice that an error handler must be provided, and this handler
   * must throw an error or provide an equivalent for the result type of the
   * previous computation.
   *
   * You can use the operator (||=) instead of this function for syntatic sugar
   *)

  val map:  'a result -> ('a -> 'b) -> 'b result
  (**
   * Apply a function to the wraped value if the result is successful
   *)

  val commit: 'a -> 'a result
  (** Indicate a successful computation *)

  (** Indicate a failure in a computation *)
  val throw: error -> 'a result

  val (&&=): 'a result -> ('a -> 'b result) -> 'b result
  (** Conditional binding operator AND *)

  val (||=): 'a result -> (error -> 'a result) -> 'a result
  (** Conditional binding operator OR *)

  val (&&|): 'a result -> ('a -> 'b) -> 'b result
  (** Conditional binding operator MAP *)

  val (/>): unit result -> (unit -> 'b result) -> 'b result
  (**
   * Blocking semicolon operator.
   * It waits for the evaluation of unit result and ignore imediately ignore it.
   * The right-hand-side must be a thunk (a function that expects unit).
   *
   * It can be used to chain thunks in a meaningful way like:
   *   let puts s () =
   *     print_endline s;
   *     commit ()
   *
   *   let main =
   *     puts "Hello" ()     />
   *     puts "Blocking"     />
   *     puts "Computations"
   *)

  val (//>): unit result -> 'a result -> 'a result
  (**
   * Non blocking semicolon operator.
   * It chains the completion of unit result with the next in the sequence.
   *
   * It can be used to chain thunks in a meaningful way like:
   *   let puts s =
   *     assynchronous_puts s
   *     >>= commit
   *
   *   let main =
   *     puts "Hello"         //>
   *     puts "Non-blocking"  //>
   *     puts "Computations"
   *)

  (** Idiomatic monad interface for the result type *)
  module Monad : Monad
end


module type Promise = sig
  include Result

  type 'a state
  type 'a promise
end
