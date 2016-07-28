open Lwt
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
module MyResult = Sugar_lwt.Result.Make(MyError)

(* Start using them *)
open MyError
open MyResult

(* We're only printing messages to the screen here.
 *
 *  Notes
 *    - Notice the specific Lwt type hinting.
 *    - It is just the full form of the type "unit result"
 *)
let print_message m: unit state Lwt.t =
  Lwt_unix.sleep (Random.float 3.)
  >>= fun () ->
  Lwt_log.notice m
  >>= commit

(* Do some computation and return a list, if it is successful *)
let load_list n: int list result =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  commit new_list


let error_handler e: string result =
  match e with
  | Resource_not_found -> commit "recovered failure"
  | _ -> throw e

let main (): unit result =
  print_message "1 - Concurrent threads" //>
  print_message "2 - Concurrent threads" //>
  print_message "3 - Concurrent threads" //>
  print_message "4 - Concurrent threads" //>
  load_list 10
  &&| List.length
  &&= fun _len ->
  throw Resource_not_found
  ||= error_handler
  &&= fun _ ->
  let message = "You can nearly do anything you want here." in
  print_message message


let _ =
  Random.self_init ();
  Lwt_main.run (main ())
