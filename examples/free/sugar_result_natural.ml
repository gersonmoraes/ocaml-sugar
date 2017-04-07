type error
  = Prefix of string
  | Prefix2 of unit

module L1 = struct
  type error = unit
end


(*
  We are trying to define a natural transformation to work with
  Result types, making the context of groupped errors "auto compatible"

  But it seems we need to do the same for general results
  It might be useful having all errors with serialization functions
  We could use S expressions as a conventional format, or use to_strong

  Maybe we can inject this definition directly inside the Result P module
*)
module NaturalError = struct
  type src = L1.error
  type dst = error

  let apply e = Prefix e
  let reverse = function
    | Prefix e -> e
    | _ -> failwith "could not reverse natural transformation for this error"
end

(* Possible ppx to abstract the pattern above *)
module NaturalError2 = struct
  type src = L1.error
  type dst = error [@@transform_with "Prefix"]
end

module type Reversible = sig
  type src
  type dst

  val apply: src -> dst
  val reverse: dst -> src
end

module type Reversible2 = sig
  type src
  type dst

  val apply: src -> dst
  val reverse: dst -> src option
end

module NaturalError2 = struct
  type src = L1.error
  type dst = error

  let apply e = Prefix e
  let reverse = function
    Prefix e -> Some e
    | _ -> None
end



(* How to use NaturalError *)
module Dsl (Ctx:Spec.S.Context) (Error:Spec.S.NaturalError) = struct
  open Ctx

  module Result = Result.For(Free) (Error)

  let puts s =
    Puts (s, id) |> lift

  let get_line () =
    GetLine id |> lift
end












(*  *)
