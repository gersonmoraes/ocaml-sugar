open Sugar_types

(**
  A functor that implements the blocking interface.

  This functors produces a module following the interface {!Sugar_types.Result}.
*)
module Make (UserError:Error) : Sugar_types.Result
  with type error = UserError.t =
struct
  type 'a result = ('a, UserError.t) Pervasives.result
  type error = UserError.t

  let return v = Ok v

  let throw e = Error e

  let bind_if r f =
    match r with
      | Error e -> Error e
      | Ok v -> f v

  let bind_unless r f =
    match r with
    | Error e -> f e
    | Ok v -> Ok v

  let map r f =
    match r with
    | Error e -> Error e
    | Ok v -> Ok (f v)


  module Infix = struct
    let (>>=) = bind_if
    let (>>|) = map
    let (>>>) x y =
      match x, y with
      | (Error e, _) -> Error e
      | _ -> y

    let (>---------) = bind_unless

    let (<*>) f x =
      f
      >>= fun f' ->
      x
      >>= fun x' ->
      return (f' x')

    let (<$>) f x = map x f
  end

  let unwrap = function
    | Ok r -> r
    | Error _ -> invalid_arg "Could not unwrap value from result"

  let unwrap_or f r =
    match r with
    | Ok r -> r
    | Error e -> f e

  let expect r msg =
    match r with
    | Ok r -> r
    | Error _ -> invalid_arg msg


  let (>>=) = bind_if

  let (>>) x y =
    x
    >>= fun () ->
    y

  module Monad : Sugar_types.Monad
    with type 'a t = 'a result =
  struct
    type 'a t = 'a result

    let return = return
    let (>>=) = bind_if
  end

  module For(M: Sugar_types.Monad) = struct
    include Sugar_promise.Make (M) (UserError)
  end

  module type NaturalError = sig
    type dst
    val apply: error -> dst
    val reverse: dst -> error option
  end

  module With(M: Sugar_types.Monad) (Natural:NaturalError) = struct
    module CompleteNatural = struct
      type src = error
      include Natural
    end
    include Sugar_monadic_with_natural.Make (M) (CompleteNatural)
  end
end


(* module type NaturalError = sig
  type src
  type dst

  val apply: src -> dst
  val reverse: dst -> src
end

module MakeWith(Natural: NaturalError) = struct
  module Error = struct
    type error = Natural.dst
  end
end *)
