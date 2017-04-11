(* open Sugar *)

open Sexplib.Std
open Sugar.DSL.Utils
open Printf

(* module Result = Sugar.MakeResult (Sugar.DSL.Error) *)
(* open Result *)

module X = struct
  open Sugar.DSL.CoreResult

  module Errors = struct
    type t = Issue1 | Issue2 [@@deriving sexp]
  end

  type Sugar.DSL.Error.t += Error of Errors.t
  (* exception Error of Errors.t *)

  module Core = struct
    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next
    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end
  open Core

  module Spec = Sugar.DSL.SpecFor (Core)

  module Api (Ctx:Spec.S.Context) = struct
    let x_puts s = Puts (s, id) |> Ctx.lift
    let x_get_line () = GetLine id |> Ctx.lift
  end

  module Runner = struct
    let run = function
      | Puts (s, f) -> print_endline s; return () |> f
      | GetLine f -> read_line () |> return |> f
    let debug = function
      | Puts (s, f) ->
          printf "X.Puts: %s\n" s; return () |> f
      | GetLine f ->
          printf "X.GetLine: "; read_line () |> return |> f
  end
end



module Y = struct
  open Sugar.DSL.CoreResult

  module Core = struct
    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next
    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end

  open Core

  module Spec = Sugar.DSL.SpecFor (Core)

  module Api (Ctx:Spec.S.Context) = struct
    let y_puts s = Puts (s, id) |> Ctx.lift
    let y_get_line () = GetLine id |> Ctx.lift
  end

  (* Error definitions *)
  module Errors = struct
    type t = Unexpected of string | Not_found [@@deriving sexp]
  end
  (* exception Error of Errors.t *)
  type Sugar.DSL.Error.t += Error of Errors.t

  module Runner = struct
    open Errors

    let throw (e:Errors.t) = throw (Error e)

    let run = function
      | Puts (s, f) -> print_endline s; return () |> f
      | GetLine f -> read_line () |> return |> f
    let debug = function
      | Puts (s, f) ->
          printf "Y.Puts: %s\n" s; throw (Unexpected "Could not print your msg") |> f
          (* printf "Puts: %s\n" s; return () |> f *)
      | GetLine f ->
          printf "Y.GetLine: "; read_line () |> return |> f
  end
end



module X_and_Y = struct
  (*
    Here, we're including new modules Core, Spec and Natural
  *)
  include Sugar.DSL.Combine (X.Core) (Y.Core)

  module Runner = struct
    let run = function
      | Core.Case1 cmd -> X.Runner.run cmd
      | Core.Case2 cmd -> Y.Runner.run cmd

    let debug = function
      | Core.Case1 cmd -> X.Runner.debug cmd
      | Core.Case2 cmd -> Y.Runner.debug cmd
  end

  module Api (Ctx: Spec.S.Context) = struct
    include X.Api (Natural.Proxy1.For(Ctx))
    include Y.Api (Natural.Proxy2.For(Ctx))
  end
end


module Context = Sugar.DSL.ContextFor(X_and_Y.Core)
open Context

open Result
open Result.Infix

module Api = X_and_Y.Api (Context)

(*
  TODO
    - We have a precedence issue with our semicolon operator: (/>)
    - We need to "take" the combinator (>>) to act as the semicolon
    - We should make the "discard operation" as the operator (>>>)
*)
open Api

let program1 =
  x_puts "What's your name?" >>
  x_get_line ()
  >>= fun name ->
  x_puts (name ^ ", have a nice day") >>
  y_puts "Let's test some errors?! Type something."
  >---------
  ( function
    | X.Error e ->
      ( match e with
        | X.Errors.Issue1 -> return ()
        | X.Errors.Issue2 -> return ()
      )
    | Y.Error e ->
      ( x_puts
        ( "Could not handle this error: " ^
          ( Sexplib.Sexp.to_string_hum @@ Y.Errors.sexp_of_t e )
        )
      )
    | _ -> assert false
  ) >>
  x_get_line ()
  >---------
  ( fun _ ->
    x_puts "The computation resulted in an unexpected error" >>
    return "recovered"
  )
  >>= fun line ->
  x_puts ("You said: " ^ line)


let () =
  Context.run X_and_Y.Runner.debug (Result.unwrap program1)
