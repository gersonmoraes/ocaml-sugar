
(*
  We are trying to define a natural transformation to work with 
  Result types, making the context of groupped errors "auto compatible" 
  
  But it seems we need to do the same for general results
  It might be useful having all errors with serialization functions
  We could use S expressions as a conventional format, or use to_strong 
  
  Maybe we can inject this definition directly inside the Result P module 
*)
module type NaturalError = struct 
  type src = L1.error
  type dst = grouped_error
  
  let apply e = Prefix e
  let reverse = function 
    | Prefix e -> e
    | _ -> failwith "could not reverse natural transformation for this error" 
end

(* Possible ppx to abstract the pattern above *)
module type NaturalError = struct 
  type src = L1.error
  type dst = error [@@natural using:"Prefix"] 
end

module type Reversible = sig 
  type src
  type dst
  
  val apply: src -> dst
  val reverse: dst -> src
end 