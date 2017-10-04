# Sugar — On demand error handling layers

%%VERSION%%

Sugar is a small monadic library that tries to simplify the use of error aware expressions with a monadic interface. Check out the the [documentation][docs] online for more information.



## Main features

- Unified interface to describe a result monad
- Module builders to customize the monadic interface to your project
- Works well on top of threading libraries like Lwt or Async
- Exception handling is supported to some degree with the *strict* interfaces



## Quick start

1. Create an isolated module to describe your errors.
2. Use one of Sugar's module builders to create a custom `Result` module for your project. *This module will implement a clean DSL to help you create error aware computations*.
3. Open and start using these modules.



### Example

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



### Warning about type hinting

The result monad will create a new type `'a result` that describes  the result of any computation inside your project. If you use `Lwt`, this type would take the form:

```ocaml
type 'a result = ('a, Errors.t) Result.result Lwt.t
```

The `Result.result` type above comes from the [result][result package] package. In recent versions of OCaml (>= 4.03), this type will be an alias for `Pervasives.result`.



[docs]: https://gersonmoraes.github.io/ocaml-sugar/doc/latest/sugar/Sugar/index.html
[result package]: https://github.com/janestreet/result
