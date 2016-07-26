Sugar
==========

A small monadic library to simplify the creation of error handling layers in
OCaml projects.


Introduction
------------

Learn the basics of [OCaml Sugar](https://dl.dropboxusercontent.com/u/9364054/OCaml/OCaml_Sugar.pdf) in the introduction slides. A somewhat deep explanation of some of its concepts are
available [here](https://dl.dropboxusercontent.com/u/9364054/OCaml/Bindings_Condicionais.pdf).


Conventions
-----------

- Error aware computations *from the same layer* should always return the same ```error type```.
- Create your own ```Result``` module with the functor  ```Result.Make```.
- Chained expressions don't usually need to use parentesis
- Developers should avoid hinting created modules as ```Sugar```'s generic modules,
as that makes OCaml hide implementation details of you error types.


Code example
------------

Example of basic usage of Sugar to build an Error Handling Layer

It uses a parametrized module to build sugar syntatic and useful functions
that only make sense in your project.  We're hinting adding type hinting result
to make clear what's going on.


```ocaml


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
let load_list n: int list result =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  commit new_list

let error_handler e: string result =
  match e with
  | Resource_not_found -> commit "recovered failure"
  | _ -> throw e

let computation_chain: unit result =
  print_message "Error aware computations" />
  load_list 10
  &&= fun l ->
  &&| List.length
  &&= fun len ->
  throw Resource_not_found
  ||= error_handler
  &&= print_message
```
