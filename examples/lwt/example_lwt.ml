open Lwt

(* Example of basic usage of Sugar to build an Error Handling Layer
 *
 * It uses a parametrized module to build sugar syntatic and useful functions
 * that only make sense in your project.
 *)


(* Create a module with your own "error" type *)
module MyError =
struct
  type error =
    | Resource_not_found
    | Unexpected of string
end

(* Generate your error handling layer with your parametrized Result module *)
module MyResult = Sugar_lwt.Result.Make(MyError)

(* Start using them *)
open MyError
open MyResult

(* We're only printing messages to the screen here.
 *
 *  Notes
 *    - Notice the specific Lwt type hinting.
 *    - It is just the full form of the type "unit promise"
 *)
let puts m: unit result Lwt.t =
  Lwt_unix.sleep (Random.float 3.)
  >>= fun () ->
  Lwt_log.notice m
  >>= commit

(* Do some computation and return a list, if it is successful *)
let load_list n: int list promise =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  commit new_list


let error_handler e: string promise =
  match e with
  | Resource_not_found -> commit "recovered failure"
  | _ -> throw e

let main (): unit promise =
  puts "1 - Concurrent threads" //>
  puts "2 - Concurrent threads" //>
  puts "3 - Concurrent threads" //>
  puts "4 - Concurrent threads" //>
  load_list 10
  &&| List.length
  &&= fun _len ->
  throw Resource_not_found
  ||= error_handler
  &&= fun _ ->
  let message = "You can nearly do anything you want here." in
  puts message


let _ =
  Random.self_init ();
  Lwt_main.run (main ())
