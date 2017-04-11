module DSL = Sugar.Dsl
open DSL.Prelude

open Printf

module Terminal = struct

  module Core = struct
    open DSL.CoreResult

    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next

    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end
  open Core

  module Spec = DSL.SpecFor (Core)

  module Dsl (Ctx:Spec.S.Context) = struct
    open Ctx

    let puts s =
      Puts (s, id) |> lift

    let get_line () =
      GetLine id |> lift
  end

  module Errors = struct
    type t = string
  end

  type exn += Error of Errors.t

  module Runner = struct
    open DSL.CoreResult

    let throw e = Error e

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


module MyMachine = DSL.ContextForRuntime(Terminal)
module TerminalDsl = Terminal.Dsl (MyMachine)

open TerminalDsl

open MyMachine

open Result
open Result.Infix

let program1 () =
  puts "What's your name?" >>
  get_line ()
  >>= fun name ->
  puts (name ^ ", have a nice day")

let () =
  MyMachine.run_error_aware Terminal.Runner.run program1
