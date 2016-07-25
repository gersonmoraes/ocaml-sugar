open Async.Std

open Sugar.Types

(* Example of basic usage of Sugar to build an Error Handling Layer
 *
 * It uses a parametrized module to build sugar syntatic and useful functions
 * that only make sense in your project.
 *
 * We're hinting the functions result to make clear what's going on. But you
 * can effectively remove them, because we're also using a concise result type
 * for computations: ('a result).
 *)


(* Create a module with your own "error" type *)
module MyError =
struct
  type error =
    | Resource_not_found
    | Unexpected of string
end

(* Generate your error handling layer with your parametrized Result module *)
(* module MyResult = Result.Make(MyError) *)
(* Sugar_lwt *)
module MyResult = Sugar_async.Result.Make(MyError)

(* Start using them *)
open MyError
open MyResult

(* We're only printing messages to the screen here.
 *
 *  Notes
 *    - Notice the specific Lwt type hinting.
 *    - It is just the full form of the type "unit result"
 *)
 let logger =
  Log.create `Info [Log.Output.stdout ()] `Raise


let flush_logger () =
  Log.flushed logger
  >>= fun () ->
  commit ()

let print_message m: unit state Deferred.t =
  after (Core.Time.Span.of_sec (Random.float 3.))
  >>= fun _ ->
  Log.info logger "%s" m;
  flush_logger ()
  /> commit ()


(* Do some computation and return a list, if it is successful *)
let load_list n: int list result =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  commit new_list

let computation_failed _length: 'a result =
  throw Resource_not_found

let error_handler e: string result =
  match e with
  | Resource_not_found -> commit "recovered failure"
  | _ -> throw e

let main_handler: unit state Deferred.t =
  print_message     "1 - Parallels bindings"
  /> print_message "2 - Parallels bindings"
  /> print_message "3 - Parallels bindings"
  /> print_message "4 - Parallels bindings"
  /> load_list 10
  (* &&= fun () -> load_list 10 *)
  &&= fun l ->
  commit (List.length l)
  &&= computation_failed
  ||= error_handler
  &&= fun _ ->
  let message = "You can nearly do anything you want here." in
  print_message message
  &&= flush_logger


let _ =
  Random.self_init ();
  let _ = Deferred.bind main_handler (fun _ -> exit 0) in
  Core.Std.never_returns (Scheduler.go ())
