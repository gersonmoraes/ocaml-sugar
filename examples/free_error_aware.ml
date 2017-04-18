open Sugar.Dsl
open Sexplib.Std

open Printf

module Terminal = struct

  module Errors = struct
    type t = string [@@deriving sexp]
  end

  module Algebra = struct
    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next

    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end

  module Spec = SpecFor (Algebra)
  include Library.Init (Spec) (Errors)
  
  open Algebra

  module New (C:Spec.Context) = struct
    include Init(C)
    open C

    let puts s =
      Puts (s, id) |> lift

    let get_line () =
      GetLine id |> lift
  end

  module Runner = struct
    let run = function
      | Puts (s, f) ->
          print_endline s;
          commit f ()
      | GetLine f ->
          commit f @@ read_line ()

    let debug = function
      | Puts (s, f) ->
          printf "Puts: %s\n" s;
          commit f ()
      | GetLine f ->
          printf "GetLine: ";
          commit f @@ read_line ()
  end
end


module Context = ContextForRuntime(Terminal)
module MyTerminal = Terminal.New (Context)

open MyTerminal

open Context
open Infix

let program1 () =
  puts "What's your name?" >>
  get_line ()
  >>= fun name ->
  puts (name ^ ", have a nice day")

let () =
  Context.reduce Terminal.Runner.run program1
