(*
  The FreeMonad implementation in this module was translated from Haskell by
     André Nathan (https://github.com/andrenth)
*)

module type Functor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Monad = sig
  type 'a t
  val return: 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type FreeMonad = sig
  type 'a f

  type 'a t =
    | Pure of 'a
    | Free of ('a t) f

  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val lift : 'a f -> 'a t
  val iter : ('a f -> 'a) -> 'a t -> 'a
  val map  : ('a -> 'b) -> 'a t -> 'b t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>)  : 'a t -> 'b t -> 'b t
    val (/>)  : unit t -> 'b t -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module Free (F : Functor) : FreeMonad with type 'a f = 'a F.t = struct
  type 'a f = 'a F.t

  type 'a t =
    | Pure of 'a
    | Free of ('a t) F.t

  let return x = Pure x

  let rec bind f = function
    | Pure y -> f y
    | Free y -> Free (F.map (fun z -> bind f z) y)

  let lift f = Free (F.map return f)

  let flip f = fun x y -> f y x

  let rec iter f = function
    | Pure x -> x
    | Free m -> f (F.map (iter f) m)

  let map f m = bind (fun x -> return (f x)) m

  let (>>=) m f = bind f m

  module Infix = struct
    let (>>=) m f = bind f m
    let (>>) x y = bind (fun _ -> y) x
    let (/>) x y = bind (fun () -> y) x
    let (>>|) m f = map f m
  end
end