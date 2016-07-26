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
module MyResult = Sugar.Result.Make(MyError)

(* Start using them *)
open MyResult
open MyError
open Printf

(* We're only printing messages to the screen here *)
let print_message m: unit result =
  print_endline m;
  commit ()

(* Do some computation and return a list, if it is successful *)
let load_list n: int list result =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  commit new_list

let error_handler e: string result =
  match e with
  | Resource_not_found -> commit "recovered failure"
  | _ -> throw e

let _ =
  print_message "We are extensively using a user defined result type" />
  load_list 10
  &&| List.length
  &&= fun len ->
  throw (Unexpected (sprintf "List length invalid: %d" len))
  ||= error_handler
  &&= (fun recovered ->
         print_message "This will NOT be printed"        />
         print_message "The previous error_handler"      />
         print_message "can't catch 'Unexpected' errors"
      )
  ||= fun e ->
  commit "Recover from any error"
  &&= print_message
