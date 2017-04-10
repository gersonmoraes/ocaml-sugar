(* open Sugar *)

module Machine = Sugar_machine_with_shared_errors
open Machine.Utils
open Printf

module Result = Sugar.MakeResult (Machine.Error)
open Result

module X = struct

  (* it could be less annoying if this was defined by the user *)
  module Errors = struct
    type t = Issue1 | Issue2
  end
  
  type Machine.Error.t += Error of Errors.t

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
    module Result = Result.For(Ctx.Free)
    let puts s = Puts (s, id) |> Ctx.lift
    let get_line () = GetLine id |> Ctx.lift
  end

  module Runner = struct
    let run = function
      | Puts (s, f) -> print_endline s; return () |> f
      | GetLine f -> read_line () |> return |> f
    let debug = function
      | Puts (s, f) ->
          printf "X.Puts: %s\n" s; return () |> f
      | GetLine f ->
          printf "X.GetLine: ";
          (* throw (Error.X_Error ()) |> f *)
          read_line () |> return |> f
  end
end



module Y = struct

  (* it could be less annoying if this was defined by the user *)
  module Errors = struct
    type t = Issue1 | Issue2
  end
  
  type Machine.Error.t += Error of Errors.t

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
    (* The result module should be already inside the context *)
    let puts s = Puts (s, id) |> Ctx.lift
    let get_line () = GetLine id |> Ctx.lift
  end

  module Runner = struct
    let run = function
      | Puts (s, f) -> print_endline s; return () |> f
      | GetLine f -> read_line () |> return |> f
    let debug = function
      | Puts (s, f) ->
          printf "Y.Puts: %s\n" s; throw (Error.Y_Error ()) |> f
          (* printf "Puts: %s\n" s; return () |> f *)
      | GetLine f ->
          printf "Y.GetLine: "; read_line () |> return |> f
  end
end













module CustomDsl = struct
  module L = Machine.Combine (X.Core) (Y.Core)

  let run = function
    | L.Core.Case1 cmd -> X.Runner.run cmd
    | L.Core.Case2 cmd -> Y.Runner.run cmd

  let debug = function
    | L.Core.Case1 cmd -> X.Runner.debug cmd
    | L.Core.Case2 cmd -> Y.Runner.debug cmd

  module MyMachine = Machine.ForLanguage(L.Core)
  module X = X.Dsl (L.T1.For(MyMachine))
  module Y = Y.Dsl (L.T2.For(MyMachine))
end


open CustomDsl
open Result
open Result.Infix

(*
  TODO
    - We have a precedence issue with our semicolon operator: (/>)
    - We need to "take" the combinator (>>) to act as the semicolon
    - We should make the "discard operation" as the operator (>>>)
*)

let program1 =
  X.puts "What's your name?" >>
  Y.get_line ()
  >>= fun name ->
  X.puts (name ^ ", have a nice day") >>
  Y.puts "Let's test some errors?! Type something."
  >---------
  ( function
    | X.Error e ->
      ( match e with
        | X.Errors.Issue1 -> return ()
        | X.Errors.Issue2 -> return () 
      )
    | Y.Error e -> return ()
    | _ -> assert false
  ) >>
  X.get_line ()
  >---------
  ( fun _ ->
    X.puts "The computation resulted in an unexpected error" >>
    return "recovered"
  )
  >>= fun line ->
  Y.puts ("You said: " ^ line)


let () =
  MyMachine.run debug (Result.unwrap program1)
