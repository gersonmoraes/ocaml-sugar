
(*
  Example of basic usage of Sugar to build an Error Handling Layer

  It uses a parametrized module to build sugar syntatic and useful functions
  that only make sense in your project.

  We're hinting the functions result to make clear what's going on. But you
  can effectively remove them, because we're also using a concise result type
  for computations: ('a result).
*)

(* Create a module with your own "error" type *)
module MyError =
struct
  type t =
    | Something of string
    | Unexpected of exn

  let panic e =
    Unexpected e
end

module MyMonad = struct

  module Deferred = Async.Deferred

  let try_with f =
    Async.try_with f
end

(* Generate your error handling layer with your parametrized Result module *)
module MyResult = Sugar.Strict.JsPromise.Make (MyError) (MyMonad)

(* Start using them *)
open MyResult
open MyResult.Infix
open MyError
open Printf

(* We're only printing messages to the screen here *)
let puts m: unit result =
  print_endline m;
  return ()

(* Do some computation and return a list, if it is successful *)
let load_list n: int list result =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  return new_list

let main () =
  ( puts "Hello World"                   ) >>lazy
  ( raise (Failure "Something happened") ) >>>lazy
  ( List.length <$> load_list 10 )
  >---------
  ( function
    | Unexpected (e) -> return 50
    | e -> throw e
  )
  >>=
  ( fun len ->
    ( puts "This will be printed"                   ) >>lazy
    ( puts ("And length is " ^ (string_of_int len)) ) >>lazy
    ( puts "We're also using a semicolon operation" )
  )
  >>lazy
  ( load_list 10 )
  >>| List.length
  >>=
  ( fun len ->
    throw (Something (sprintf "List length invalid: %d" len))
  )
  >---------
  ( fun e ->
    puts "Recover from any error"
  )
  >>= fun _ ->
  exit 0



let _ =
  let _ = main () in
  Async.Scheduler.go ()
