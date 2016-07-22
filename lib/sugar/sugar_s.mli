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
