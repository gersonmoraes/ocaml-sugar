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
module LwtResult = Sugar_lwt.Result.Make(MyError)

(* Start using them *)
open MyError
open LwtResult

(* We're only printing messages to the screen here *)
let print_message m: unit result Lwt.t =
  Lwt_log.notice m
  >>= commit

(* Do some computation and return a list, if it is successful *)
let load_list n: int list result Lwt.t =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  commit new_list

let computation_failed _length: 'a result Lwt.t =
  throw Resource_not_found

let error_handler e: string result Lwt.t =
  match e with
  | Resource_not_found -> commit "recovered failure"
  | _ -> throw e

let main: unit result Lwt.t =
  print_message "We are extensively using a user defined result type"
  &&= fun () ->
  load_list 10
  &&= fun l ->
  commit (List.length l)
  &&= computation_failed
  ||= error_handler
  &&= fun _ ->
  let message = "You can nearly do anything you want here." in
  print_message message


let _ =
  Lwt_main.run (main)
