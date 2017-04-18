open Sexplib.Std

open Sugar.Dsl

open Printf

module X = struct

  module Errors = struct
    type t = Issue1 | Issue2 [@@deriving sexp]
  end

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

  include Library (Spec) (Errors)

  module type Api = sig
    include Partials
    module Errors = Errors

    val puts: string -> unit result
    val get_line: unit -> string result
  end

  module New (C:Spec.S.Context) : Api
    with type 'a result = 'a C.result =
  struct
    include Init (C)
    module Errors = Errors

    let puts s = Puts (s, id) |> C.lift
    let get_line () = GetLine id |> C.lift
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
      | Puts _ -> printf "[X.Puts] "; run cmd
      | GetLine _ -> printf "[X.GetLine] "; run cmd
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

  module Errors = struct
    type t = Unexpected of string | Not_found [@@deriving sexp]
  end

  include Library (Spec) (Errors)

  module type Api = sig
    include Partials
    module Errors = Errors

    val puts: string -> unit result
    val get_line: unit -> string result
  end

  module New (C:Spec.S.Context) : Api
    with type 'a result = 'a C.result =
  struct
    include Init (C)
    module Errors = Errors

    let puts s = Puts (s, id) |> C.lift
    let get_line () = GetLine id |> C.lift
  end

  module Runner = struct
    open Prelude.Runner

    let run = function
      | Puts (s, f) ->
          rollback f @@ Errors.Unexpected "Could not print your msg"
      | GetLine f ->
          commit f @@ read_line ()

    let debug cmd =
      match cmd with
      | Puts _ -> printf "[Y.Puts] "; run cmd
      | GetLine _ -> printf "[Y.GetLine] "; run cmd
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

  module Errors = struct
    type t = XY_error [@@deriving sexp]
  end

  include Library (Spec) (Errors)

  module type Api = sig
    include Partials
    module Errors = Errors

    module X : X.Api with type 'a result := 'a result
    module Y : Y.Api with type 'a result := 'a result
  end

  module New (Ctx: Spec.S.Context) : Api
    with type 'a result = 'a Ctx.result =
  struct
    include Init (Ctx)
    module Errors = Errors

    module X = X.New (Natural.Proxy1.For(Ctx))
    module Y = Y.New (Natural.Proxy2.For(Ctx))

   (*
   module X : X.Api
      with type 'a result = 'a Ctx.result =
      X.New (Natural.Proxy1.For(Ctx))

    module Y : Y.Api
      with type 'a result = 'a Ctx.result =
      Y.New (Natural.Proxy2.For(Ctx))
    *)
  end
end


module Context = ContextFor(X_and_Y.Algebra)

open Context
open Infix

module Terminal = X_and_Y.New (Context)

open Terminal

let program1 () =
  return "hello" >>>
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
    | Y.Error e ->
        X.puts ("Error in Y: " ^ (Y.string_of_error e))
    | _ -> assert false
  ) >>
  X.get_line ()
  >---------
  ( fun _ ->
    X.puts "The computation resulted in an unexpected error" >>
    return "recovered"
  )
  >>= fun line ->
  X.puts ("You said: " ^ line) >>
  Y.puts "hello world"
  >---------
  ( fun e ->
    ( match e with
      | Y.Error error_y -> X.puts ("Error in Y: " ^ ( Y.string_of_error error_y ))
      | _ -> throw e
    )
  )

let program2 () =
  X.puts "Hello"
  >---------
  ( fun e ->
    return ()
  )
  >>= fun () ->
  X.puts "World"


let () =
  Context.run_and_unwrap X_and_Y.Runner.debug program1
