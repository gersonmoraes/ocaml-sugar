(**
   This module defines minimalistic interfaces for all Sugar modules
 *)
open Generic

module type NaturalError = sig
  type src
  type dst

  val apply: src -> dst
  val reverse: dst -> src option
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
    module MyResult = Sugar.Result.Make (MyError)
    
    module MyResult2 = Sugar.Promise.Make (Lwt) (MyError)
    module MyResult2 = MyResult.For (Lwt)
  </code>
*)
module type Error = sig
  (**
    You should implement this type according to your project.
    This could be any type, including strings or unit.
  *)
  type t
end

(**
  This interface specifies an error handling layer for monadic computations.

  Sugar result modules work with any monad.
  
  <code>
    module MyMonad = struct
      type 'a monad = 'a Lwt.t
      let return = Lwt.return
      let (>>=) = Lwt.bind
    end
    module MyResult = Sugar.Promise.Make (MyMonad) (MyError)
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
        return ()
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
     Similar to {{!Result.return} Result.return}
  *)
  val return: 'a -> 'a promise


  (**
    Similar to {{!Result.throw} Result.throw}
  *)
  val throw: error -> 'a promise

  module Infix : sig

  (**
    Similar to {{!Result.(>>|)} Result.(>>|)}
  *)
  val (>>|): 'a promise -> ('a -> 'b) -> 'b promise

  (** Applicative combinator for map *)
  val (<$>): ('a -> 'b) -> 'a promise -> 'b promise
  
  (** Applicative combinator for parallel execution of function and operand *)
  val (<*>): ('a -> 'b) promise -> 'a promise -> 'b promise

  (**
    Broom combinator 
    
    Used to introduce an error handler block to "clean errors".
    
    There's a secret message behind the form of this combinator.
    It has the same number of characters sufficient for the whole block 
    in the next line. For example:
    
    <code>
    let program1 () =
      do_something () 
      >---------
      ( fun e ->
        return ()
      )
      
    let program2 () =
      do_something () 
      >---------
      ( function
        e -> return ()
      )
    </code>
    
    So beyond the clean aesthetics similar to markdown, we are
    implying that a developer should never handle errors in an open
    anonymous function.
  *)
  val (>---------): 'a promise -> (error -> 'a promise) -> 'a promise


  (**
    Ignore operator.
    
    Use this operator to ignore the previous result 
    and return the next instruction.
  *)
  val (>>>): 'a promise -> 'b promise -> 'b promise

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

  (**
    Bind combinator
    
    If the computation in the left is successful, the operator will 
    Take the inner value and feed it to the function in the right. This is an 
    alias for the function [bind_if].
    
    If the computation in the left failed, the operator will propagate the error, 
    skipping the function completely.
  *)
  val (>>=): 'a promise -> ('a -> 'b promise) -> 'b promise


 (**
   Semicolon combinator. 
   
   Like the standard semicolon in OCaml, ";", the previous operation needs 
   to evaluate to a unit promise.
 *)
 val ( >> ) : unit promise -> 'b promise -> 'b promise

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

    You can use the operator {{!(>---------)} (>---------} instead of this function for syntatic sugar
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
    Return a value in a successful computation.

    This function should be used with its counterpart, [throw]
  *)
  val return: 'a -> 'a result


  (**
    Return an error as the result of a computation.

    Like the [return] function, [throw] helps you hide the internals of your
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
          return 10
        else
          throw "something bad happend"
    </code>

    You could also not describe your errors at all for some time, and
    use the {!Sugar.Option} module to create error aware computations, like:

    <code>
      open Sugar.Option
      let run (): string result =
        if true then
          return "hello world"
        else
          throw ()
    </code>

   *)
  val throw: error -> 'a result

  module Infix : sig


  (**
    Broom combinator 
    
    Used to introduce an error handler block to "clean errors".
    
    There's a secret message behind the form of this combinator.
    It has the same number of characters sufficient for the whole block 
    in the next line. For example:
    
    <code>
    let program1 () =
      do_something () 
      >---------
      ( fun e ->
        return ()
      )
      
    let program2 () =
      do_something () 
      >---------
      ( function
        e -> return ()
      )
    </code>
    
    So beyond the clean aesthetics similar to markdown, we are
    implying that a developer should never handle errors in an open
    anonymous function.
  *)
  val (>---------): 'a result -> (error -> 'a result) -> 'a result


  (**
    Combinator for map with semantic similar to bind

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
       | Some n -> Some (double n)

      (* using the default >>= combinator *)
      let twenty =
       Some 10
       >>= fun n ->
       return (double n)

     (* using the map combinator *)
     let twenty =
       Some 10
       >>| double
    </code>
  *)
  val (>>|): 'a result -> ('a -> 'b) -> 'b result


  (**
    Ignore operator.
    
    Use this operator to ignore the previous result 
    and return the next instruction.
  *)
  val (>>>): 'a result -> 'b result -> 'b result

  val (>>=): 'a result -> ('a -> 'b result) -> 'b result


  val (<$>): ('a -> 'b) -> 'a result -> 'b result
  val (<*>): ('a -> 'b) result -> 'a result -> 'b result

  end

  (**
    Bind combinator
    
    If the computation in the left is successful, the operator will 
    Take the inner value and feed it to the function in the right. This is an 
    alias for the function [bind_if].
    
    If the computation in the left failed, the operator will propagate the error, 
    skipping the function completely.
  *)
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
        return ()

      let main =
        puts "Hello" ()     />
        puts "Blocking"     />
        puts "Computations"
      </code>
     *)
    val (>>): unit result -> 'b result -> 'b result

    (**
      Create a new result module based on the current one, but wrapped around a monad.
    *)
    module For : functor (UserMonad:Monad) -> Promise
      with type error := error
      and type 'a monad := 'a UserMonad.t
end
