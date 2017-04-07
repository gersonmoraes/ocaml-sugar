open Sugar

module Machine = Sugar_machine_error_aware
open Machine.Utils
open Printf


module X = struct

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



module Y = struct

  module Error = struct
    type error = string
  end

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


module X_and_Y = struct
  module R = Machine.Combine (X.Core) (Y.Core)

  module Error = struct
    type error
      = X_error of X.Core.Result.error
      | Y_error of Y.Core.Result.error
  end

  module Core = struct
    type 'a t = 'a R.Core.t
    let map = R.Core.map
    module Result = Sugar_result.Make(Error)
  end
  module Spec = Machine.SpecFor(Core)
  
  module Natural = struct 
    module ProxyX = R.Natural.Proxy1 
    module ProxyY = R.Natural.Proxy2
    
    (* TODO:
         We need refined signatures to merge errors.
         We can only create a merge operation between 
         Natural transformations of errors if the two 
         Modules are related to the current language.
    *)
    
    (* This pattern needs to be simplified *)
    module ErrorX = Spec.Error (struct
      type src = X.Error.error
      let apply e = X_error e
      let reverse v = function 
        | X_error e = Some e
        | _ -> None
    end)
    
    module ErrorY = Spec.Error(struct
      type src = Y.Error.error
      let apply e = Y_error e
      let reverse v = function
        | Y_error e = Some e
        | _ -> None
    end)
    end

  module Dsl(Ctx: Spec.S.Context) (E:Spec.S.ContextError) = struct
    module Result = Core.Result.With (Ctx.Free) (E)
    
    (* Conversion tools *)
    open Natural
    
    (* TODO: Can we include the context error inside the translation context? *)

    module X = X.Dsl (ProxyX.For(Ctx)) (ErrorX.For(E))
    module Y = Y.Dsl (ProxyY.For(Ctx)) (ErrorY.For(E))
    
    open Result
    
    (* Play time *)
    let todo () =
      X.puts "x: hello" />
      Y.puts "y: hello" />
      X.get_line ()
      >>= fun line ->
      Y.puts ("y: line returned from x:" ^ line) />
      throw (X_error ())
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
