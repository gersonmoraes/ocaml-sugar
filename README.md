
Sugar
==========

[Sugar](https://www.ocamlplace.com/sugar) is a modern Model Driven Development library for OCaml.


Principles
------------

- Follow a bare bones declarative aproach, complemented with meta programming.
- Error aware expressions everywhere, to produce safer code
- OCaml lean and clean with a unified monadic interface.
- Don't write a program, write a description of a program that can be interpreted in many ways, using a a *Free Monad*.
- Create pluggable DSLs that can be mixed in a number of ways



## Clean and lean

**Notes on error aware expressions**

- Error aware computations *from the same layer* should always return the same ```error type```.
- Chained expressions don't usually need to use parentesis
- Error handlers always should be inside a block. Here is [why](https://www.ocamlplace.com/sugar/presentation.html#11).

**An example using error aware expressions with Lwt**

1. Create an isolated module for your errors.
2. Generate a ```Result``` module for your errors.
3. Open the recently created modules.

Notice the hinting for function return types. We're using ```'a result```
instead of  ```'a score Lwt.t``` for convenience.


```ocaml
module Errors = struct
  type t = Not_available | Unexpected of string
end

module Result = Sugar.Promise.Make (Lwt) (Errors)

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
  >>= fun len ->
  Printf.printf "The len is %d\n" len;
  return ()

let () =
  Lwt_main.run ( unwrap (program ()) );;
```

# Write a DSL

Check the `examples` directory to see some implementations of toy DSLs. More information can be found in the [presentation](https://ocamlplace.com/sugar/presentation.html) for the project.
