(**
  Sugar's main module.

  Sugar is a monadic library to help you create an error handling layer.
  It creates simple functions and combinators to uniform and simplify the
  manipulation of error aware computations, like using the result type with
  asynchronous computations.

  To start using Sugar, the first thing you need is to define your errors in an
  isolated module:
  <code>
  module MyError =
  struct
    type error = unit
  end
  </code>

  After that, you have to tell Sugar what kind of error handling layer you want.
  You do that by using one of Sugar's functors. They will generate a Result module
  that understands your project, and help you use error aware expressions.

  For example, you can use the blocking interface using this code:
  <code>
  module MyResult = Sugar.MakeResult(MyError)
  </code>

  This is it. You can start using this modules in your project. Bellow, there's an
  example of Sugar in action.
  <code>
  open MyResult
  open MyError

  (* A simple error aware function *)
  let get_list () =
    commit [1; 2; 3]

  (* Recovering from failed string computations *)
  let error_handler = function
    | Resource_not_found -> commit "recovered failure"
    | _ -> throw e

  (* Nested computations can be handled with Sugar combinators *)
  let computation_chain: unit result =
    get_list ()
    &&| List.length
    &&= fun _len ->
    throw Resource_not_found
    ||= error_handler
    &&= fun _msg ->
    commit ()
  </code>
*)

(** This module defines minimalistic interfaces for all Sugar modules *)
module Types = Sugar_types

(** Implements the Sugar interface for the option type *)
module Option = Sugar_option

(** A functor that implements the blocking interface *)
module MakeResult = Sugar_result.Make

(** A functor that implements the asynchronous interface  *)
module MakePromise = Sugar_promise.Make

module Machine = Sugar_machine
