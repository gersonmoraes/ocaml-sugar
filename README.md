
Sugar
==========

[Sugar](https://www.ocamlplace.com/sugar) is a small monadic library for error aware computations.

## How to use it

1. Create an isolated module to describe your errors.
2. Use one of Sugar's module builders to create a custom `Result` module for your project. *This module will implement a clean DSL to help you create error aware computations*.
3. Open and start using these modules.



### An example

The main idea of using this library is to help you use error aware expressions everywhere.

In the code bellow, we're using type hinting to make it clear the type `result` is used to represent the current monad.


```ocaml
module Errors = struct
  type t = Not_available | Unexpected of string
end

module Result = Sugar.Promise.Make (Errors) (Lwt)

open Errors

open Result
open Result.Infix

let program () : unit result =
  return [1; 2; 3]
  >>| List.length
  >---------
  ( function
    | Not_available -> return 0
    | Unexpected s  -> return 0
  )
  >>=
  ( fun len ->
    Printf.printf "The len is %d\n" len;
    return ()
  )

let () =
  Lwt_main.run ( unwrap (program ()) );;
```



Sugar creates a type `result` that represents  the result of a computation in this project. The translation of `result` in this example to stantard OCaml is:

```ocaml
type 'a result = ('a, Errors.t) Result.result Lwt.t
```
