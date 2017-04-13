open Sugar.Dsl

open Printf

module Terminal = struct

  module Algebra = struct
    open Prelude.Algebra

    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next

    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end
  open Algebra

  module Spec = SpecFor (Algebra)

  module New (Ctx:Spec.S.Context) = struct
    open Ctx

    let puts s =
      Puts (s, id) |> lift

    let get_line () =
      GetLine id |> lift
  end

  module Errors = struct
    type t = string
  end

  include ErrorFor(Errors)

  module Runner = struct
    open Prelude.Runner

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


module Context = ContextForRuntime(Terminal)
module MyTerminal = Terminal.New (Context)

open MyTerminal
open Context

open Result
open Result.Infix

let program1 () =
  puts "What's your name?" >>
  get_line ()
  >>= fun name ->
  puts (name ^ ", have a nice day")

let () =
  Context.run_error_aware Terminal.Runner.run program1
