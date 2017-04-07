open Sugar

module Machine = Sugar_machine_error_aware
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

  module Dsl(Ctx: Spec.S.Context) (E:Spec.S.Error) = struct
    module Result = Core.Result.With (Ctx.Free) (E)
    open Ctx
  
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
    module Result = Sugar.MakeResult (Error)
    open Result
    type 'f t =
      | Puts2 of string * ('f, unit result) next
      | GetLine2 of ('f, string result) next

    let map f = function
      | Puts2 (s, g) -> Puts2 (s, f @ g)
      | GetLine2 g -> GetLine2 (f @ g)
  end
  open Core

  module Spec = Machine.SpecFor (Core)

  module Dsl(Ctx: Spec.S.Context) (E:Spec.S.Error) = struct
    module Result = Core.Result.With (Ctx.Free) (E)
    open Ctx

    let puts s =
      Puts2 (s, id) |> lift

    let get_line () =
      GetLine2 id |> lift
  end

  module Runner = struct
    open Result
    let run = function
      | Puts2 (s, f) -> print_endline s; return () |> f
      | GetLine2 f -> read_line () |> return |> f

    let debug = function
      | Puts2 (s, f) ->
          printf "Puts: %s\n" s; return () |> f
      | GetLine2 f ->
          printf "GetLine: ";
          read_line () |> return |> f
  end
end
(*
let _ =
  (module Terminal.Core:Machine.Language),
  (module Terminal:Machine.Runtime) *)


module Terminals = struct
  module R = Machine.Combine (Terminal.Core) (Terminal2.Core)

  module Error = struct
    type error
      = Terminal_error of Terminal.Core.Result.error
      | Terminal2_error of Terminal2.Core.Result.error
  end

  module Core = struct
    type 'a t = 'a R.Core.t
    let map = R.Core.map

    module Result = Sugar_result.Make(Error)
  end
  module Spec = Machine.SpecFor(Core)

  module Dsl(Ctx: Spec.S.Context) (E:Spec.S.Error) = struct
    module Result = Core.Result.With (Ctx.Free) (E)
  end
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
