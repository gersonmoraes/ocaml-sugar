(* open Sugar *)

module Machine = Sugar_machine_error_aware
open Machine.Utils
open Printf


module Terminal = struct

  (* module Computation = Sugar.MakeResult
    (struct
      type error = unit
    end) *)

  (*
     If we add errors and results to the Core module, we can figure them out on
     combine modules as well, right?
  *)
  module Core = struct
    module Error = struct
      type error = string
    end
    (* Our new convention *)
    module Result = Sugar.MakeResult (Error)
    open Result

    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next

    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end
  open Core

  module Spec = Machine.SpecFor (Core)

  module Dsl (Ctx:Spec.S.Context) = struct
    open Ctx
    module Result = Result.For(Free)

    let puts s =
      Puts (s, id) |> lift

    let get_line () =
      GetLine id |> lift
  end

  module Runner = struct
    open Core.Result

    let run = function
      | Puts (s, f) -> print_endline s; return () |> f
      | GetLine f -> read_line () |> return |> f

    let debug = function
      | Puts (s, f) ->
          printf "Puts: %s\n" s; return () |> f
      | GetLine f ->
          printf "GetLine: ";
          read_line () |> return |> f
  end
end
(*
let _ =
  (module Terminal.Core:Machine.Language),
  (module Terminal:Machine.Runtime) *)



module MyMachine = Machine.For(Terminal)
module TerminalDsl = Terminal.Dsl (MyMachine)

open TerminalDsl
open Result

let program1 =
  puts "What's your name?" />
  get_line ()
  >>= fun name ->
  puts (name ^ ", have a nice day")

let () =
  MyMachine.run Terminal.Runner.run (Result.unwrap program1)
