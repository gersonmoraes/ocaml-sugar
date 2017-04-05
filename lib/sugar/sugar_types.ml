(**
   This module defines minimalistic interfaces for all Sugar modules
 *)

(** Common monadic signature *)
module type Monad = sig
  type 'a monad
  val return: 'a -> 'a monad
  val (>>=): 'a monad -> ('a -> 'b monad) -> 'b monad
end


(**
  Conventional module type to define the errors inside a project.
  This is how Sugar understands the error handling layer of a project.

  Like:
  <code>
    module MyError = struct
      type error = Not_found | Invalid_arg of string
    end
  </code>

  This module might be used to create blocking or asynchronous error handling
  layers, using the Sugar functors, like:
  <code>
    module MyResult = Sugar.MakeResult(MyError)
  </code>
*)
module type Error = sig
  (**
    You should implement this type according to your project.
    This could be any type, including strings or unit.
  *)
  type error
end


(** Monadic interface for result types *)
module type Result = sig

  (**
    Error definition from your project
  *)
  type error


  (**
    An alias for the result type in the stdlib
  *)
  type 'a result = ('a, error) Pervasives.result


  (**
    Apply the binding only if the computation was successful.
    You can use the operator {{!(&&=)} &&=} instead of this function for syntatic sugar
   *)
  val bind_if:  'a result -> ('a -> 'b result) -> 'b result


  (**
    Apply the binding only if the computation failed.

    Notice that an error handler must be provided, and this handler
    must throw an error or provide an equivalent for the result type of the
    previous computation.

    You can use the operator {{!(||=)} ||=} instead of this function for syntatic sugar
   *)
  val bind_unless: 'a result -> (error -> 'a result) -> 'a result


  (**
     Apply a function to the result of a successful computation. This function
     makes it ease to work with non error aware functions.

     Example:
     <code>
      open Sugar.Option

      let twenty =
        map (Some 10) (fun n -> n + n)
     </code>

     You could also use the combinator {{!(>>|)} >>|} for syntatic sugar.
   *)
  val map:  'a result -> ('a -> 'b) -> 'b result

  (**
    Commit a successful computation.

    The main reason for this function is to abstract the differences from the
    internals of your result type. It provides a standard interface to create
    successful values to both blocking and non-blocking computations. So, you
    can make your computations asynchronous without refactoring a lot of code.
    You could also change between the option and result types and keep large
    parts of your code intact.

    This function should be used with its counterpart, [throw]
  *)
  val commit: 'a -> 'a result


  (**
    Return an error as the result of a computation.

    Like the [commit] function, [throw] helps you hide the internals of your
    result type and keep a clean code.

    If you are still at the beginning of your project, and don't have your
    errors defined yet, this function still is a great help. For example,
    the code bellow have the same usage as the function [failwith], but is a lot
    safer.

    <code>
      module MyResult = Sugar.MakeResult (struct error = string end)
      open MyResult
      let run (): int result =
        if true then
          commit 10
        else
          throw "something bad happend"
    </code>

    You could also not describe your errors at all for some time, and
    use the {!Sugar.Option} module to create error aware computations, like:

    <code>
      open Sugar.Option
      let run (): string result =
        if true then
          commit "hello world"
        else
          throw ()
    </code>

   *)
  val throw: error -> 'a result

  module Infix : sig



  val (>---------): 'a result -> (error -> 'a result) -> 'a result

  (*
    Conditional binding operator AND. An alias for {{!bind_if} bind_if}.

    This is the main reason Sugar can simplify the usage of error aware
    expressions. For example, say you are working with a project with a lot of
    error aware expressions, and between those, you have these functions:

    <code>
      let computation () =
        let value = ... in
        Ok value

      let transform value =
        let new_value = ... in
        Ok new_value
    </code>

    There are various ways you could use the functions above in OCaml.
    Someone could write this code in idiomatic OCaml:

    <code>
      let run () =
        match computation () with
        | Error e -> Error e
        | Ok v ->
          begin
            match transform v with
            | Error e -> Error e
            | Ok v ->
              begin
                print_endline "everything ok";
                Ok ()
              end
          end
    </code>

    But we believe this code is not very expressive. And as your code base grows
    and you start using error aware expressions everywhere, it tends to bloat
    your code with pattern matching. You could also avoid using the typesystem
    to handle the errors and rely on exceptions, but exceptions literally bypass
    compiler and may easily break your code at runtime.

    If you are using Sugar, it is very likely your final version would look
    like:

    <code>
      let run () =
        computation ()
        &&= transform
        &&= fun v ->
        print_endline "everything ok";
        commit ()
    </code>

    For a more diverse example, look at the {!Sugar} module.
  *)
  (* val (&&=): 'a result -> ('a -> 'b result) -> 'b result *)


  (*
    Conditional binding operator OR. An alias for {{!bind_unless} bind_unless}.

    This function lets you assign an error handler inside a chained error
    aware expression.

    For a more diverse example, look at the {!Sugar} module.
  *)
  (* val (||=): 'a result -> (error -> 'a result) -> 'a result *)


  (**
    Conditional binding operator MAP

    As its name sugests, this is an alias for the function {{!map} map}.
    Its intended use is to help integrate with functions that are not error
    aware.

    For example, considere the function [let double x = x + x] in the code
    fragments bellow:

    <code>
     open Sugar.Option

     (* example without Sugar *)
     let twenty =
       match Some 10 with
       | None -> None
       | Some n -> double n

      (* using the default &&= combinator *)
      let twenty =
       Some 10
       &&= fun n ->
       commit (double n)

     (* using the map combinator *)
     let twenty =
       Some 10
       >>| double
    </code>
  *)
  val (>>|): 'a result -> ('a -> 'b) -> 'b result


  (*
    Non blocking semicolon operator.
    It chains the completion of unit result with the next in the sequence.

    It can be used to chain thunks in a meaningful way like:
    <code>
    let puts s =
      asynchronous_puts s
      >>= commit

    let main =
      puts "Hello"         //>
      puts "Non-blocking"  //>
      puts "Computations"
    </code>
   *)
  (* val (//>): unit result -> 'a result -> 'a result *)

  val (>>): 'a result -> 'b result -> 'b result

  val (>>=): 'a result -> ('a -> 'b result) -> 'b result

  end

  val (>>=): 'a result -> ('a -> 'b result) -> 'b result
  (**
    Unwraps the successful result as a normal value in the threading monad.
    If the value is not successful, it will raise an Invalid_arg exception.
  *)
  val unwrap: 'a result -> 'a


  (**
    Unwraps the successful result as a value in the threading monad.
    Different from [unwrap], you can assign an error handler to be
    executed if the computation failed. Example:
    <code>
    let run () =
      get_data ()
      |> unwrap_or (fun _ -> "default")
    </code>
  *)
  val unwrap_or: (error -> 'a) -> 'a result -> 'a


  (**
    Extracts a successful value from an computation, or raises and Invalid_arg
    exception with the defined parameter.
  *)
  val expect: 'a result -> string -> 'a


    (**
      Blocking semicolon operator.
      It waits for the evaluation of unit result and ignore imediately ignore it.
      The right-hand-side must be a thunk (a function that expects unit).

      It can be used to chain thunks in a meaningful way like:
      <code>
      let puts s () =
        print_endline s;
        commit ()

      let main =
        puts "Hello" ()     />
        puts "Blocking"     />
        puts "Computations"
      </code>
     *)
    val (/>): unit result -> 'b result -> 'b result


end


(**
  This interface specifies an error handling layer for asynchronous.
  computations.

  Sugar works with any concurrent threadling library. The most basic functor
  to create an asynchronous computation might be used like this
  <code>
    module MyMonad = struct
      type 'a monad = 'a Lwt.t
      let return = Lwt.return
      let (>>=) = Lwt.bind
    end
    module MyResult = Sugar.MakePromise (MyMonad) (MyError)
  </code>

  If you install Sugar with opam, and you have Lwt or Async are installed, you
  you will get sugar sub-libraries, respectively "sugar.lwt" and "sugar.async".
  With one of these libraries, you can use a shorter version:
  <code>
    module MyResult = Sugar_lwt.MakeResult   (MyError)
    module MyResult = Sugar_async.MakeResult (MyError)
  </code>
*)
module type Promise = sig

  (** Error definition imported from your project *)
  type error

  (** Low level result type *)
  type 'a result = ('a, error) Pervasives.result

  (**
    This is a virtual type that will be translated to your asynchronous
    library's main type.
   *)
  type 'a monad


  (**
    High level result type, created to simplify type hinting.
    It hides two things: your choice of asynchronous library and the relation
    with your project's error definition.

    For example, considere this function:
    <code>
      let run () : unit promise =
        commit ()
    </code>

    The actual could be something like:
    <code>
      (unit, error) Pervasives.result Lwt.t
    </code>
  *)
  type 'a promise = 'a result monad


  (**
     Similar to {{!Result.bind_if} Result.bind_if}
   *)
  val bind_if:  'a promise -> ('a -> 'b promise) -> 'b promise


  (**
     Similar to {{!Result.bind_unless} Result.bind_unless}
   *)
  val bind_unless: 'a promise -> (error -> 'a promise) -> 'a promise


 (**
    Similar to {{!Result.map} Result.map}
  *)
  val map:  'a promise -> ('a -> 'b) -> 'b promise


  (**
     Similar to {{!Result.commit} Result.commit}
  *)
  val commit: 'a -> 'a promise


  (**
    Similar to {{!Result.throw} Result.throw}
  *)
  val throw: error -> 'a promise

  module Infix : sig

  (**
    Similar to {{!Result.(>>|)} Result.(>>|)}
  *)
  val (>>|): 'a promise -> ('a -> 'b) -> 'b promise



   val (>---------): 'a promise -> (error -> 'a promise) -> 'a promise


  (*
     "Non blocking" semicolon operator.
     It chains the completion of unit promise with the next in the sequence.

     It can be used to chain thunks in a meaningful way like:
     <code>
       let puts s =
         asynchronous_puts s
         >>= commit

       let main =
         puts "Hello"         //>
         puts "Non-blocking"  //>
         puts "Computations"
       </code>
   *)
   (* val (//>): unit promise -> 'a promise -> 'a promise *)

   val (>>): 'a promise -> 'b promise -> 'b promise

  end

  (**
    Unwraps the successful result as a normal value in the threading monad.
    If the value is not successful, it will raise an Invalid_arg exception.
  *)
  val unwrap: 'a result monad -> 'a monad


  (**
    Unwraps the successful result as a value in the threading monad.
    Different from [unwrap], you can assign an error handler to be
    executed if the computation failed. Example:
    <code>
    let run () =
      get_data ()
      |> unwrap_or (fun _ -> Lwt.return "default")
    </code>
  *)
  val unwrap_or: (error -> 'a monad) -> 'a result monad -> 'a monad


  (**
    Extracts a successful value from an computation, or raises and Invalid_arg
    exception with the defined parameter.
  *)
  val expect: 'a result monad -> string -> 'a monad

  val (>>=): 'a promise -> ('a -> 'b promise) -> 'b promise


 (* val (/>): unit promise -> (unit -> 'b promise) -> 'b promise *)
 val ( /> ) : unit promise -> 'b promise -> 'b promise

end
