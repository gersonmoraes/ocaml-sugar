open Sugar
open Machine.Utils
open Printf


module Terminal = struct

  module Error = struct
    type error = unit
  end

  (*
     If we add errors and results to the Core module, we can figure them out on
     combine modules as well, right?
  *)
  module Core = struct

    (* Our new convention *)
    include Sugar.MakeResult (Error)

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
    module Result = For(Free)

    let puts s =
      Puts (s, id) |> lift

    let get_line () =
      GetLine id |> lift
  end

  module Runner = struct
    let run = function
      | Puts (s, f) -> print_endline s; commit () |> f
      | GetLine f -> read_line () |> commit |> f

    let debug = function
      | Puts (s, f) ->
          printf "Puts: %s\n" s; commit () |> f
      | GetLine f ->
          printf "GetLine: ";
          read_line () |> commit |> f
  end
end
(*
let _ =
  (module Terminal.Core:Machine.Language),
  (module Terminal:Machine.Runtime) *)



module Terminal2 = struct

  module Error = struct
    type error = string
  end

  (*
     If we add errors and results to the Core module, we can figure them out on
     combine modules as well, right?
  *)
  module Core = struct

    (* Our new convention *)
    include Sugar.MakeResult (Error)

    type 'f t =
      | Puts2 of string * ('f, unit result) next
      | GetLine2 of ('f, string result) next

    let map f = function
      | Puts2 (s, g) -> Puts2 (s, f @ g)
      | GetLine2 g -> GetLine2 (f @ g)
  end
  open Core

  module Spec = Machine.SpecFor (Core)

  module Dsl (Ctx:Spec.S.Context) = struct
    open Ctx
    module Result = For(Free)

    let puts s =
      Puts2 (s, id) |> lift

    let get_line () =
      GetLine2 id |> lift
  end

  module Runner = struct
    let run = function
      | Puts2 (s, f) -> print_endline s; commit () |> f
      | GetLine2 f -> read_line () |> commit |> f

    let debug = function
      | Puts2 (s, f) ->
          printf "Puts: %s\n" s; commit () |> f
      | GetLine2 f ->
          printf "GetLine: ";
          read_line () |> commit |> f
  end
end
(*
let _ =
  (module Terminal.Core:Machine.Language),
  (module Terminal:Machine.Runtime) *)


module Terminals = struct
  include Machine.Combine (Terminal.Core) (Terminal2.Core)
end

(* module Terminals. *)

module MyMachine = Machine.For(Terminal)
module TerminalDsl = Terminal.Dsl (MyMachine)
(* module TerminalDsl2 = Terminal2.Dsl (MyMachine) *)

open TerminalDsl
open Result

let program1 =
  puts "What's your name?" />
  get_line ()
  >>= fun name ->
  puts (name ^ ", have a nice day")

let () =
  MyMachine.run Terminal.Runner.run (Result.unwrap program1)
