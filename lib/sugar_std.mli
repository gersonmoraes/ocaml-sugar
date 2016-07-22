(**
 * Common monadic signature
 *)
module type Monad = sig
  type 'a m

  val return: 'a -> 'a m
  val (>>=): 'a m -> ('a -> 'b m) -> 'b m
end
