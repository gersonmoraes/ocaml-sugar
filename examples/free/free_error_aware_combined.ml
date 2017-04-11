(* open Sugar *)

open Sugar.DSL.Utils
open Printf

(* module Result = Sugar.MakeResult (Sugar.DSL.Error) *)
(* open Result *)

module X = struct
  open Sugar.DSL.CoreResult

  module Errors = struct
    type t = Issue1 | Issue2
  end

  type Sugar.DSL.Error.t += Error of Errors.t

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
    let puts s = Puts (s, id) |> Ctx.lift
    let get_line () = GetLine id |> Ctx.lift
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
    let puts s = Puts (s, id) |> Ctx.lift
    let get_line () = GetLine id |> Ctx.lift
  end

  (* Error definitions *)
  module Errors = struct
    type t = unit
  end
  type Sugar.DSL.Error.t += Error of Errors.t

  module Runner = struct
    open Errors

    let throw (e:Errors.t) = throw (Error e)

    let run = function
      | Puts (s, f) -> print_endline s; return () |> f
      | GetLine f -> read_line () |> return |> f
    let debug = function
      | Puts (s, f) ->
          printf "Y.Puts: %s\n" s; throw () |> f
          (* printf "Puts: %s\n" s; return () |> f *)
      | GetLine f ->
          printf "Y.GetLine: "; read_line () |> return |> f
  end
end



module X_and_Y = struct
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
    module X = X.Api (Natural.Proxy1.For(Ctx))
    module Y = Y.Api (Natural.Proxy2.For(Ctx))
  end
end


module Context = Sugar.DSL.ContextFor(X_and_Y.Core)
(* module Result = Sugar.DSL.CoreResult.For(Context.Free) *)

open Context

open Result
open Result.Infix

(* open Result
open Result.Infix *)

module Api = X_and_Y.Api (Context)

(*
  TODO
    - We have a precedence issue with our semicolon operator: (/>)
    - We need to "take" the combinator (>>) to act as the semicolon
    - We should make the "discard operation" as the operator (>>>)
*)

let program1 =
  Api.X.puts "What's your name?" >>
  Api.X.get_line ()
  >>= fun name ->
  Api.X.puts (name ^ ", have a nice day") >>
  Api.Y.puts "Let's test some errors?! Type something."
  >---------
  ( function
    | X.Error e ->
      ( match e with
        | X.Errors.Issue1 -> return ()
        | X.Errors.Issue2 -> return ()
      )
    | Y.Error () -> return ()
    | _ -> assert false
  ) >>
  Api.X.get_line ()
  >---------
  ( fun _ ->
    Api.X.puts "The computation resulted in an unexpected error" >>
    return "recovered"
  )
  >>= fun line ->
  Api.X.puts ("You said: " ^ line)


let () =
  Context.run X_and_Y.Runner.debug (Result.unwrap program1)
