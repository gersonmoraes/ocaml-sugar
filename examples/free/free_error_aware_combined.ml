(* open Sugar *)

module Machine = Sugar_machine_with_shared_errors
open Machine.Utils
open Printf

(* module Result = Sugar.MakeResult (Machine.Error) *)
(* open Result *)

module X = struct
  open Machine.CoreResult

  (* it could be less annoying if this was defined by the user *)
  module Errors = struct
    type t = Issue1 | Issue2
  end

  type Machine.Error.t += Error of Errors.t

  module Core = struct

    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next
    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end
  open Core

  module Spec = Machine.SpecFor (Core)

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
          printf "X.GetLine: ";
          (* throw (Error.X_Error ()) |> f *)
          read_line () |> return |> f
  end
end



module Y = struct
  open Machine.CoreResult

  module Errors = struct
    type t = Issue1 | Issue2
  end

  type Machine.Error.t += Error of Errors.t

  module Core = struct
    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next
    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end
  open Core

  module Spec = Machine.SpecFor (Core)

  module Api (Ctx:Spec.S.Context) = struct
    let puts s = Puts (s, id) |> Ctx.lift
    let get_line () = GetLine id |> Ctx.lift
  end

  module Runner = struct
    open Errors

    let run = function
      | Puts (s, f) -> print_endline s; return () |> f
      | GetLine f -> read_line () |> return |> f
    let debug = function
      | Puts (s, f) ->
          printf "Y.Puts: %s\n" s; throw (Error Issue1) |> f
          (* printf "Puts: %s\n" s; return () |> f *)
      | GetLine f ->
          printf "Y.GetLine: "; read_line () |> return |> f
  end
end



module X_and_Y = struct
  include Machine.Combine (X.Core) (Y.Core)

  module Runner = struct
    let run = function
      | Core.Case1 cmd -> X.Runner.run cmd
      | Core.Case2 cmd -> Y.Runner.run cmd

    let debug = function
      | Core.Case1 cmd -> X.Runner.debug cmd
      | Core.Case2 cmd -> Y.Runner.debug cmd
  end

  module Api (Ctx: Spec.S.Context) = struct
    module X = X.Api (T1.For(Ctx))
    module Y = Y.Api (T2.For(Ctx))
  end
end


module Context = Machine.ForLanguage(X_and_Y.Core)
module Result = Machine.CoreResult.For(Context.Free)

open Result
open Result.Infix

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
    | Y.Error e -> return ()
    | _ -> assert false
  ) >>
  Api.X.get_line ()
  >---------
  ( fun _ ->
    Api.X.puts "The computation resulted in an unexpected error" >>
    return "recovered"
  )
  >>= fun line ->
  Api.Y.puts ("You said: " ^ line)


let () =
  Context.run X_and_Y.Runner.debug (Result.unwrap program1)
