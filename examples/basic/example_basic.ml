open Sugar.Std


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
module MyResult = Result.Make(MyError)

(* Start using them *)
open MyResult
open MyError

(* We're only printing messages to the screen here *)
let print_message m: unit result =
  print_endline m;
  commit ()

(* Do some computation and return a list, if it is successful *)
let load_list n (): int list result =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  commit new_list

let computation_failed _ignored: 'a result =
  throw Resource_not_found

let error_handler e: string result =
  match e with
  | Resource_not_found -> commit "recovered failure"
  | _ -> throw e

let computation_chain: unit result =
  print_message "We are extensively using a user defined result type"
  &&= load_list 10
  &&= fun l -> commit (List.length l)
  &&= computation_failed
  ||= error_handler
  &&= fun _ ->
         print_endline "You can nearly do anything you want here.";
         print_endline "We are not restricted to result types.";
         commit ()
