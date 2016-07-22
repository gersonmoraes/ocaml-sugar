
 (* This should be refactored to the default result in OCaml >= 4.03  *)
 type ('a, 'b) std_result =
   | Ok of 'a
   | Error of 'b


 (**
  * Common monadic signature
  *)
module type Monad = sig
  type 'a monad

  val return: 'a -> 'a monad
  val (>>=): 'a monad -> ('a -> 'b monad) -> 'b monad
end

module type Error = sig
  type error
end
