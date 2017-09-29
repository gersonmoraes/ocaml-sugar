module type Functor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Monad = sig
  type 'a t
  val return: 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end


(**
  Conventional module type to define the errors inside a project.
  This is how Sugar understands the error handling layer of a project.

  Like:
  {[
    module MyError = struct
      type error = Not_found | Invalid_arg of string
    end
  ]}

  This module might be used to create blocking or asynchronous error handling
  layers, using the Sugar functors, like:
  {[
    module MyResult = Sugar.Result.Make (MyError)

    module MyResult2 = Sugar.Promise.Make (Lwt) (MyError)
    module MyResult2 = MyResult.For (Lwt)
  ]}
*)
module type Error = sig
  (**
    You should implement this type according to your project.
    This could be any type, including strings or unit.
  *)
  type t
end


module Strict = struct
  module type Error = sig
    include Error

    val panic : exn -> t
  end


  module type Monad = sig
    include Monad

    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  end
end
