Sugar
==========

A small monadic library to simplify the creation of error handling layers in
OCaml projects.


Introduction
------------

Learn the basics of [OCaml Sugar](https://dl.dropboxusercontent.com/u/9364054/OCaml/OCaml_Sugar.pdf)
in the introduction slides. A somewhat deep explanation of some of its concepts are
available [here](https://dl.dropboxusercontent.com/u/9364054/OCaml/Bindings_Condicionais.pdf).


Conventions
-----------

- Error aware computations *from the same layer* should always return the same ```error type```.
- Create your own ```Result``` module with the functor  ```Result.Make```.
- Chained expressions don't usually need to use parentesis
- Developers should avoid hinting created modules as ```Sugar```'s generic modules,
as that makes OCaml hide implementation details of you error types.


Usage Example
-------------

To use Sugar, you need to do 2 things:

1. Create an empty module with a type ```error```, called *error module*
2. Generate a ```Result``` module using your "error module".
3. Open the recently created modules.

Notice the hinting for function return types. We're using ```'a promise```
instead of  ```'a result Lwt.t``` for convenience. 


```ocaml
module MyError =
struct
  type error = unit
end

module MyResult = Sugar_lwt.Result.Make(MyError)

open MyResult
open MyError

let load_list n: int list promise =
  let l = [1; 2; 3] in
  let new_list = List.map (fun v -> v * n) l in
  commit new_list

let error_handler e: string promise =
  match e with
  | Resource_not_found -> commit "recovered failure"
  | _ -> throw e

let computation_chain: unit promise =
  load_list 10
  &&= fun l ->
  &&| List.length
  &&= fun len ->
  throw Resource_not_found
  ||= error_handler
  &&= fun _msg ->
  commit ()
```


Use Idiomatic Semicolons
-------------------------

To simplify chaind expressions that resolves to *non-useful values*, Sugar
introduces two operators that act as monadic semicolons: ```/>``` and ```//>```
(blocking and non-blocking, respectively).

#### Expressing blocking operations

The blocking semicolon allows you to rewrite expressions like:

```ocaml
let puts s =
  print_endline s; commit ()

let main =
  puts "hello"
  &&= fun () ->
  puts "world"
```

to expressions like bellow, using functions that receives unit as a parameter:

```ocaml
let puts s () =
  print_endline s; commit ()

let main =
  puts "hello" ()   />
  puts "world"      />
  puts "of"         />
  puts "semicolons"
```

#### Expressing non-blocking operations

If your expressions don't return anything meaningful and can be called out of
order, you can run them concurrently using  ```//>``` as a separator. It will
tie the termination of an expression to the next result in the chain.

In the example bellow, the messages are likely to be printed out of order.
Notice that ```main``` will only be resolved when all concurrent operations
are terminated.

```ocaml
let puts str =
  lwt () = Lwt_unix.sleep (Random.float 2) in
  lwt () = Lwt_log.notice str in
  commit ()

let main =
  puts "message 1" //>
  puts "message 2" //>
  puts "message 3" //>
  puts "message 4"
```


Asynchronous Suport
--------------------

Sugar is extensible. It comes with a generic monad interface that makes Sugar
work with any monadic threading library. It already comes with Lwt and Async
support.

In order to avoid confusion, Sugar does not overlap ```>>=``` and ```return```.
To use those, you have to open the specific threading library module.

#### Integration with Threading libraries

Sugar results can be extracted using ```unwrap``` or ```unwrap_or``` methods.
This is useful if you are working with different libraries. The example bellow
show this integration with asynchronous results:

```ocaml
module MyError = struct
  type error = Unexpected
end
module MyResult = Sugar_lwt.Result.Make(MyError)

open Lwt
open MyResult

let load_unsafe (): string Lwt.t =
  unwrap ( commit "Hello" )
  >>= return

let load_safe () : string Lwt.t =
  unwrap_or ( throw MyError.Unexpected ) (fun err -> "recovered")
  >>= return
```

#### Standard monadic interface

If you want to use the standard monad interface, you can still use the generated
module ```Result.Monad```. In the example bellow, ```return``` is an alias
for ```commit``` and ```>>=``` is an alias for ```&&=```.


```ocaml
module MyError =
struct
  type error = unit
end

module MyResult = Sugar_lwt.Result.Make(MyError)

open MyResult
open MyResult.Monad

let main : string result Lwt.t =
  return "Hello"
  >>= fun msg ->
  return (msg ^ " World")
```
