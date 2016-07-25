
(* Defining the library interface *)

module Types = Sugar_types

module Result = Sugar_result
module Monadic = Sugar_monadic


(*
module OpaqueResult = Result.Make (struct
    type error = ()
  end) *)
