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
  type error

  (* type 'a result *)
  type 'a result = ('a, error) Pervasives.result

  (**
    Apply the binding only if the computation was successful.
    You can use the operator (&&=) instead of this function for syntatic sugar
   *)
  val bind_if:  'a result -> ('a -> 'b result) -> 'b result


  (**
    Apply the binding only if the computation failed.

    Notice that an error handler must be provided, and this handler
    must throw an error or provide an equivalent for the result type of the
    previous computation.

    You can use the operator (||=) instead of this function for syntatic sugar
   *)
  val bind_unless: 'a result -> (error -> 'a result) -> 'a result

  val map:  'a result -> ('a -> 'b) -> 'b result
  (**
     Apply a function to the wraped value if the result is successful
   *)

  val commit: 'a -> 'a result
  (** Indicate a successful computation *)

  (** Indicate a failure in a computation *)
  val throw: error -> 'a result

  (** Conditional binding operator AND *)
  val (&&=): 'a result -> ('a -> 'b result) -> 'b result

  (** Conditional binding operator OR *)
  val (||=): 'a result -> (error -> 'a result) -> 'a result

  (** Conditional binding operator MAP *)
  val (&&|): 'a result -> ('a -> 'b) -> 'b result

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
  val (/>): unit result -> (unit -> 'b result) -> 'b result

  (**
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
  val (//>): unit result -> 'a result -> 'a result

  val unwrap: 'a result -> 'a
  val unwrap_or: 'a result -> (error -> 'a) -> 'a
  val expect: 'a result -> string -> 'a
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
     Apply the binding only if the computation was successful.
     You can use the operator (&&=) instead of this function for syntatic sugar
   *)
  val bind_if:  'a promise -> ('a -> 'b promise) -> 'b promise

  (**
     Apply the binding only if the computation failed.

     Notice that an error handler must be provided, and this handler
     must throw an error or provide an equivalent for the promise type of the
     previous computation.

     You can use the operator (||=) instead of this function for syntatic sugar
   *)
  val bind_unless: 'a promise -> (error -> 'a promise) -> 'a promise

 (**
    Apply a function to the wraped value if the promise is successful
  *)
  val map:  'a promise -> ('a -> 'b) -> 'b promise

  (** Indicate a successful computation *)
  val commit: 'a -> 'a promise

  (** Indicate a failure in a computation *)
  val throw: error -> 'a promise

 (** Conditional binding operator AND. *)
  val (&&=): 'a promise -> ('a -> 'b promise) -> 'b promise

  (** Conditional binding operator OR *)
  val (||=): 'a promise -> (error -> 'a promise) -> 'a promise

  (** Conditional binding operator MAP *)
  val (&&|): 'a promise -> ('a -> 'b) -> 'b promise


  (**
     Blocking semicolon operator.
     It waits for the evaluation of unit promise and ignore imediately ignore it.
     The right-hand-side must be a thunk (a function that expects unit).

     It can be used to chain thunks in a meaningful way like:
       let puts s () =
         print_endline s;
         commit ()

       let main =
         puts "Hello" ()     />
         puts "Blocking"     />
         puts "Computations"
   *)
   val (/>): unit promise -> (unit -> 'b promise) -> 'b promise


  (**
     Non blocking semicolon operator.
     It chains the completion of unit promise with the next in the sequence.

     It can be used to chain thunks in a meaningful way like:
       let puts s =
         asynchronous_puts s
         >>= commit

       let main =
         puts "Hello"         //>
         puts "Non-blocking"  //>
         puts "Computations"
   *)
   val (//>): unit promise -> 'a promise -> 'a promise

  val unwrap: 'a result monad -> 'a monad
  val unwrap_or: 'a result monad -> (error -> 'a monad) -> 'a monad
  val expect: 'a result monad -> string -> 'a monad
end
