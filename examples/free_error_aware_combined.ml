open Sexplib.Std

open Sugar.Dsl

open Printf

module X = struct

  module Errors = struct
    type t = Issue1 | Issue2 [@@deriving sexp]
  end

  include ErrorFor(Errors)

  module Algebra = struct
    open Prelude.Algebra

    type 'f t =
      | Puts of string * ('f, unit result) next
      | GetLine of ('f, string result) next

    let map f = function
      | Puts (s, g) -> Puts (s, f @ g)
      | GetLine g   -> GetLine (f @ g)
  end

  open Algebra

  module Spec = SpecFor (Algebra)

  module New (C:Spec.S.Context) = struct
    let x_puts s = Puts (s, id) |> C.lift
    let x_get_line () = GetLine id |> C.lift
  end

  module Runner = struct
    open Prelude.Runner

    let run = function
      | Puts (s, f) ->
          print_endline s;
          commit f ()
      | GetLine f ->
          commit f @@ read_line ()

    let debug cmd =
      match cmd with
      | Puts _ -> printf "X.Puts"; run cmd
      | GetLine _ -> printf "X.GetLine: "; run cmd
  end
end
(* let _ = (module X:DSL.S.Library) *)


module Y = struct
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

  module New (C:Spec.S.Context) = struct
    let y_puts s = Puts (s, id) |> C.lift
    let y_get_line () = GetLine id |> C.lift
  end

  module Errors = struct
    type t = Unexpected of string | Not_found [@@deriving sexp]
  end

  include ErrorFor(Errors)

  module Runner = struct
    open Prelude.Runner

    let run = function
      | Puts (s, f) ->
          rollback f @@ Errors.Unexpected "Could not print your msg"
      | GetLine f ->
          commit f @@ read_line ()

    let debug cmd =
      match cmd with
      | Puts _ -> printf "Y.Puts"; run cmd
      | GetLine _ -> printf "Y.GetLine: "; run cmd
  end
end


module X_and_Y = struct

  include Combine (X.Algebra) (Y.Algebra)

  module Runner = struct
    open Algebra

    let run = function
      | Case1 cmd -> X.Runner.run cmd
      | Case2 cmd -> Y.Runner.run cmd

    let debug = function
      | Case1 cmd -> X.Runner.debug cmd
      | Case2 cmd -> Y.Runner.debug cmd
  end

  module New (Ctx: Spec.S.Context) = struct
    include X.New (Natural.Proxy1.For(Ctx))
    include Y.New (Natural.Proxy2.For(Ctx))
  end
end


module Context = ContextFor(X_and_Y.Algebra)

open Context
open Infix

module Lib1 = X_and_Y.New (Context)

open Lib1

let program1 () =
  x_puts "What's your name?" >>
  y_get_line ()
  >>= fun name ->
  x_puts (name ^ ", have a nice day") >>
  y_puts "Let's test some errors?! Type something."
  >---------
  ( function
    | X.Error e ->
      ( match e with
        | X.Errors.Issue1 -> return ()
        | X.Errors.Issue2 -> return ()
      )
    | Y.Error e ->
        x_puts ("Error in Y: " ^ (Y.string_of_error e))
    | _ -> assert false
  ) >>
  x_get_line ()
  >---------
  ( fun _ ->
    x_puts "The computation resulted in an unexpected error" >>
    return "recovered"
  )
  >>= fun line ->
  x_puts ("You said: " ^ line) >>
  y_puts "hello world"
  >---------
  ( fun e ->
    ( match e with
      | Y.Error error_y -> x_puts ("Error in Y: " ^ ( Y.string_of_error error_y ))
      | _ -> throw e
    )
  )

let program2 () =
  x_puts "Hello"
  >---------
  ( fun e ->
    return ()
  )
  >>= fun () ->
  x_puts "World"


let () =
  Context.run_error_aware X_and_Y.Runner.debug program2
  (* Context.run_error_aware X_and_Y.Runner.run program1 *)
