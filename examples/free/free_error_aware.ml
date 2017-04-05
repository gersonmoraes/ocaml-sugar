open Sugar
open Machine.Utils
open Printf


module MyError = struct
  type error = unit
end
module MyResult = Sugar.MakeResult (MyError)
open MyResult

module Terminal = struct

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

  module Dsl (Ctx:Spec.S.Context) = struct
    open Ctx
    module MyResult = Sugar.Monadic (Ctx.Free) (MyError)

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

let _ =
  (module Terminal.Core:Machine.Language),
  (module Terminal:Machine.Runtime)



module MyMachine = Machine.For(Terminal)
module Dsl = Terminal.Dsl (MyMachine)


open Dsl
open MyResult
open MyResult.Infix

let program1 =
  puts "What's your name?" />
  get_line ()
  >>= fun name ->
  puts (name ^ ", have a nice day")

let () =
  MyMachine.run Terminal.Runner.run (unwrap program1)
