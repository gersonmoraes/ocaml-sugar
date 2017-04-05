open Sugar
open Machine.Utils
open Printf

module Terminal = struct

  module Core = struct
    type 'f t =
      | Puts of string * 'f unrelated
      | GetLine of ('f, string) next

    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end
  open Core

  module Spec = Machine.SpecFor (Core)

  module Runner = struct
    let run = function
      | Puts (s, f) -> print_endline s; f ()
      | GetLine f -> f (read_line ())

    let debug = function
      | Puts (s, f) ->
          printf "Puts: %s\n" s; f ()
      | GetLine f ->
          printf "GetLine: ";
          f (read_line ())
  end

  module Dsl (Ctx:Spec.S.Context) = struct
    open Ctx

    let puts s =
      Puts (s, id) |> lift

    let get_line () =
      GetLine id |> lift
  end
end

(* let _ =
  (module Terminal.Core : Generic.Functor),
  (module Terminal      : Machine.Runtime) *)


module Env = Machine.For(Terminal)
module Dsl = Terminal.Dsl (Env)

open Env.Free.Infix
open Dsl

let program =
  puts "What's your name?" />
  get_line ()
  >>= fun name ->
  puts (name ^ ", have a nice day.")


let () =
  Env.run Terminal.Runner.run program
