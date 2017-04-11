open Abstract

(** Monadic interface for result types *)
module type S = sig

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
    module For : functor (UserMonad:Monad) -> Promise.S
      with type error := error
      and type 'a monad := 'a UserMonad.t
end


(**
  A parametric module that implements the blocking interface.

  The complete documentation can be found in {!Types.Result}.
*)
module Make (UserError:Error) : S
  with type error = UserError.t =
struct
  type 'a result = ('a, UserError.t) Pervasives.result
  type error = UserError.t

  let return v = Ok v

  let throw e = Error e

  let bind_if r f =
    match r with
      | Error e -> Error e
      | Ok v -> f v

  let bind_unless r f =
    match r with
    | Error e -> f e
    | Ok v -> Ok v

  let map r f =
    match r with
    | Error e -> Error e
    | Ok v -> Ok (f v)


  module Infix = struct
    let (>>=) = bind_if
    let (>>|) = map
    let (>>>) x y = bind_if x (fun _ -> y)
    let (>---------) = bind_unless

    let (<*>) f x =
      f
      >>= fun f' ->
      x
      >>= fun x' ->
      return (f' x')

    let (<$>) f x = map x f
  end

  let unwrap = function
    | Ok r -> r
    | Error _ -> invalid_arg "Could not unwrap value from result"

  let unwrap_or f r =
    match r with
    | Ok r -> r
    | Error e -> f e

  let expect r msg =
    match r with
    | Ok r -> r
    | Error _ -> invalid_arg msg


  let (>>=) = bind_if
  let (>>) x y = bind_if x (fun () -> y)

  module Monad : Abstract.Monad
    with type 'a t = 'a result =
  struct
    type 'a t = 'a result

    let return = return
    let (>>=) = bind_if
  end

  module For(M: Abstract.Monad) = struct
    include Promise.Make (M) (UserError)
  end
end
