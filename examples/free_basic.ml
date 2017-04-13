open Sugar.Dsl
open Printf

module Terminal = struct

  module Algebra = struct
    open Prelude.Algebra

    type 'f t =
      | Puts of string * 'f unrelated
      | GetLine of ('f, string) next

    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end
  open Algebra

  module Spec = SpecFor (Algebra)

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

  module New (Ctx:Spec.S.Context) = struct
    open Ctx

    let puts s =
      Puts (s, id) |> lift

    let get_line () =
      GetLine id |> lift
  end
end

(* let _ =
  (module Terminal.Algebra : Generic.Functor),
  (module Terminal      : Machine.Runtime) *)


module Context = ContextFor(Terminal.Algebra)
module Lib1 = Terminal.New (Context)

open Context.Free.Infix
open Lib1

let program () =
  puts "What's your name?" />
  get_line ()
  >>= fun name ->
  puts (name ^ ", have a nice day.")


let () =
  Context.run Terminal.Runner.run program
