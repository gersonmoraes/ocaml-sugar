(**
 * This module defines minimalistic interfaces for all Sugar modules
 *)
(*
type ('a, 'b) Pervasives.result =
  | Ok of 'a
  | Error of 'b *)
 (** This should be refactored to the default result in OCaml >= 4.03  *)


 (** Common monadic signature *)
module type Monad = sig
  type 'a monad
  val return: 'a -> 'a monad
  val (>>=): 'a monad -> ('a -> 'b monad) -> 'b monad
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
  type error

  (* type 'a result *)
  type 'a result = ('a, error) Pervasives.result

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

  (* Idiomatic monad interface for the result type *)
  (* module Monad : Monad *)

  val unwrap: 'a result -> 'a
  val unwrap_or: 'a result -> (error -> 'a) -> 'a
  val expect: 'a result -> string -> 'a
end

(** Module that represents an assynchronous error aware computation. *)
module type Promise = sig

  type error

  type 'a result = ('a, error) Pervasives.result
  (* Core result type *)

  type 'a monad
  (** Type that is translated to specific threading library's monad *)

  type 'a promise = 'a result monad
  (** High level result type *)

  val bind_if:  'a promise -> ('a -> 'b promise) -> 'b promise
  (**
   * Apply the binding only if the computation was successful.
   * You can use the operator (&&=) instead of this function for syntatic sugar
   *)

  val bind_unless: 'a promise -> (error -> 'a promise) -> 'a promise
  (**
   * Apply the binding only if the computation failed.
   *
   * Notice that an error handler must be provided, and this handler
   * must throw an error or provide an equivalent for the promise type of the
   * previous computation.
   *
   * You can use the operator (||=) instead of this function for syntatic sugar
   *)

  val map:  'a promise -> ('a -> 'b) -> 'b promise
  (**
   * Apply a function to the wraped value if the promise is successful
   *)

  val commit: 'a -> 'a promise
  (** Indicate a successful computation *)

  (** Indicate a failure in a computation *)
  val throw: error -> 'a promise

  val (&&=): 'a promise -> ('a -> 'b promise) -> 'b promise
  (** Conditional binding operator AND *)

  val (||=): 'a promise -> (error -> 'a promise) -> 'a promise
  (** Conditional binding operator OR *)

  val (&&|): 'a promise -> ('a -> 'b) -> 'b promise
  (** Conditional binding operator MAP *)

  val (/>): unit promise -> (unit -> 'b promise) -> 'b promise
  (**
   * Blocking semicolon operator.
   * It waits for the evaluation of unit promise and ignore imediately ignore it.
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

  val (//>): unit promise -> 'a promise -> 'a promise
  (**
   * Non blocking semicolon operator.
   * It chains the completion of unit promise with the next in the sequence.
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

  (* type 'a state *)
  (** A successful or error value *)

  (** TODO: É necessário adicionar suporte a exceções via Monad *)
  val unwrap: 'a result monad -> 'a monad
  val unwrap_or: 'a result monad -> (error -> 'a monad) -> 'a monad
  val expect: 'a result monad -> string -> 'a monad
end
