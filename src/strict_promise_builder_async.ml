open S.Params
open Result

(**
  This module is similar to {{!Sugar__Promise_builder}Sugar.Promise}.
  It lets you create a result monad on top of an arbitrary monad.

  The main difference is that the functions in this module were created to
  recognize unexpected exceptions, and require you to provide some mecanism to
  recover from that. This is done with the signatures for {{!Sugar.S.Params.Strict_error}
   strict error} and {{!Sugar.S.Params.Strict_monad} strict monad}.


  An example:
  {[
  module MyError = struct
    type t = A | B | Unexpected of exn

    let panic e = Unexpected e
  end

  module MyMonad = struct
    type 'a t = 'a Lwt.t
    let return = Lwt.return
    let (>>=) = Lwt.(>>=)
    let catch = Lwt.catch
  end

  module MyResult = Sugar.Strict.Promise.Make (MyError) (MyMonad)
  ]}

  Notice that the signature for the required strict monad is the same as the Lwt
  library. That means, you can just plug it in:
  {[
    module MyResult = Sugar.Strict.Promise.Make (MyError) (Lwt)
  ]}
*)

(**
  A parametric module that implements the monadic interface for values.
  The complete documentation can be found in {!Sugar.S.Promise}.
*)
module Make (UserError:Strict_error) (Async:Async)
: S.Strict_promise
  with
    type error := UserError.t
    and type 'a monad := 'a Async.Deferred.t
    and type 'a value = ('a, UserError.t) Result.result
    and type 'a result = (('a, UserError.t) Result.result Async.Deferred.t) lazy_t
=
struct
  include UserError
  module UserMonad = Async.Deferred

  type 'a monad = 'a UserMonad.t
  type 'a value = ('a, UserError.t) Result.result
  type 'a result = 'a value monad lazy_t

  (* open UserMonad *)

  let return v = lazy ( UserMonad.return (Ok v) )
  let throw e = lazy ( UserMonad.return (Error e) )

  let (>>=) (r:'a monad lazy_t) (f:'a -> 'b monad lazy_t) : 'b monad lazy_t =
    let open UserMonad in
    ( Async.try_with
        ( fun () ->
          Lazy.force r
          >>= fun v ->
          Lazy.force (f v)
        )
      >>= function
      | Ok v -> return v
      | Error e -> return (Error (UserError.panic e))
    )
    |> fun v ->
    lazy v

  let bind r f =
    r
    >>= function
    | Error e -> throw e
    | Ok v -> f v

  let bind_unless r f =
    r
    >>= function
    | Ok v -> return v
    | Error e -> f e

  let map r f =
    r
    >>= function
    | Error e -> throw e
    | Ok v -> return (f v)

  module Infix = struct
    let (>>=) = bind

    let (>>|) = map

    let (>>) x y = bind x (fun () -> Lazy.force y)

    let (>>>) x y = bind x (fun _ -> Lazy.force y)

    let (>>>=) = UserMonad.(>>=)

    let (>---------) = bind_unless

    let (<*>) (f: ('a -> 'b) result) (x:'a result) : 'b result =
      f
      >>= fun f' ->
      x
      >>= fun x' ->
      return (f' x')

    let (<$>) f x = map x f
  end


  let unwrap (r: 'a result) : 'a monad =
    let open UserMonad in
    ( Lazy.force r
      >>= function
      | Result.Ok v -> (UserMonad.return v)
      | Result.Error e -> raise (Invalid_argument "Could not unwrap value")
    )

  let unwrap_or (f: UserError.t -> 'a monad) (r: 'a result) : 'a monad =
    let open UserMonad in
    ( Lazy.force r
      >>= function
      | Result.Ok v -> UserMonad.return v
      | Result.Error e -> f e
    )

  let expect msg r =
    let open UserMonad in
    ( Lazy.force r
      >>= function
      | Result.Ok v -> UserMonad.return v
      | Result.Error e -> raise (Invalid_argument msg)
    )

  let ok_or_else (f:unit -> 'a result) (r:'a option result) : 'a result =
    let (>>=) = UserMonad.(>>=) in
    ( Lazy.force r
      >>= function
      | Result.Ok (Some v) -> UserMonad.return (Ok v)
      | Result.Ok None -> Lazy.force (f ())
      | Result.Error e -> UserMonad.return (Error e)
    )
    |> fun v ->
    lazy v


  let (>>=) = bind

  module NoExceptions = Promise_builder.Make (UserError) (UserMonad)
end
