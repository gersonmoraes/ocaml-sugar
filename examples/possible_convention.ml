open Sugar.Dsl

open Printf

module Terminal = struct

  module Algebra = struct
    open Prelude.Algebra
    
    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next
  end
  
  open Algebra
  
  module Functor = struct
    type 'a t = 'a Algebra.t
    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g -> GetLine (f @ g)
  end

  module Spec = SpecFor (Functor)
  
  module Errors = struct
    type t = string
  end
  
  include ErrorFor (Errors)

  module New (C:Spec.S.Context) = struct
    open C

    let puts s =
      Puts (s, id) |> lift

    let get_line () =
      GetLine id |> lift
  end
  
  module Runner = struct
    open Prelude.Runner

    let run = function
      | Puts (s, f) -> print_endline s; return () |> f
      | GetLine f -> read_line () |> return |> f
Of a number

    let debug = function
      | Puts (s, f) ->
          printf "Puts: %s\n" s; return () |> f
      | GetLine f ->
          print_endline "GetLine: ";
          read_line () |> return |> f
  end
end


module Context = ContextForRuntime(Terminal)
module Lib = Terminal.New (Context)

open Lib
open Context

let program1 () =
  puts "What's your name?" >>
  get_line ()
  >>= fun name ->
  puts (name ^ ", have a nice day")

let () =
  MyMachine.run_error_aware Terminal.Runner.run program1
