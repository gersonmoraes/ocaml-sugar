open Abstract

let id = fun x -> x

(** Function composition *)
let (@) f g = fun v -> f (g v)

(**
  In order to use error aware expressions mixing different DSLs,
  we need to make sure all DSLs are written using a compatible result type.
  That means using a shared open type for errors.

  We are using the [exn] type for this. But there are some grounding conventions:

   - Each DSL must define only one public constructor for the shared type.
   - The constructor must be called Error, so users will handle errors matching X.Error, Y.Error, and so on.
   - Different error cases must be handled with a polymorphic type, internal to each DSL
   - There must be a public function `string_of_error` for this internal error type.

  For simplicity, we provide helpers to automate this pattern.

  <code>
  module X = struct
    ...

    module Errors = struct
      type t = Failure1 | Failure2 [@@deriving sexp]
    end
    include Sugar.Dsl.ErrorFor (Errors)

    ...
  end
  </code>
*)
module Error = struct
  type t = exn
end

module Prelude = struct

  module Algebra = struct
    type 'a result = ('a, exn) Pervasives.result

    (**
      A specification for the "next continuation".

      Each instruction in our language must specify the type for the next
      continuation in the chain. Some instructions return "unit", some return
      a function. In all cases, this need to be defined.

      But the type "next" makes this trivial. The code bellow define an
      instruction that returns a string option.
      <code>
        type 'f t =
          GetData of ('f, string option) next
      </code>

      To use this expression, the developer needs to provide a function ['f] that
      can handle a [string option].

      If your continuation needs to be more complex and return, for example, a
      string option, an int, and a float, this could be done with:
      <code>
        type 'f t =
          GetData of ('f, string option -> int -> float) next
      </code>
    *)
    type ('f, 'args) next = 'args -> 'f

    (**
      Specifies that the continuation ['f] is not related to the current
      instruction.

      This type is syntatic sugar for the type [next].
    *)
    type 'f unrelated = ('f, unit) next
  end

  module CoreResult = Result.Make (Error)

  module Runner = struct
    open CoreResult

    let return = CoreResult.return

    let commit (f:'a result -> 'b) (v: 'a) = f (return v)
  end
end

open Prelude

module S = struct

  module type CoreResult = Result.S
    with type error = exn

  (**
    This describes the signature for an opaque, debuggable error.
    It supposed to be used with the ErrorFor builder.
  *)
  module type Errors = sig
    type t

    val t_of_sexp : Sexplib.Type.t -> t
    val sexp_of_t : t -> Sexplib.Type.t
  end

  (**
    This describes the signature for parametric module ErrorFor
  *)
  module type ErrorForSpec = sig

    (**
      Library internal error
    *)
    type error

    (**
      Constructor for the shared error type
    *)
    exception Error of error

    (**
      Convert the library internal error to string
    *)
    val string_of_error: error -> string

    (**
      Throw a library error
    *)

    val rollback: ('a Prelude.CoreResult.result -> 'b) -> error -> 'b
  end

  (**
    A translation context for DSLs writers.

    This signature specifies the minimum requirements to instanciate a module
    containing all helper functions associated to an algebra. They requirements are:
      - A FreeMonad
      - A natural transformation to this FreeMonad
      - A lift function tied to the algebra (to chain an instruction)
      - A return function to yield any OCaml value.

    The translation of contexts does some cool things:
      - As a DSL writer, you get a clean, typed view of the "running environment"
      - You don't need much to translate contexts of the DSL you use.
      - All specification about compatibility is type safe and automated
      - It is easy to compose a big DSL from smaller ones
  *)
  module type Context = sig
    include Natural

    module Free : FreeMonad
      with type 'a f = 'a dst

    type 'a free   = 'a Free.t

    (**
      Schedule the execution of an instruction in the current program
    *)
    val lift: 'a src -> 'a Free.t

    include Promise.S
      with type error := exn
       and type 'a monad := 'a Free.t


     val run: ('a dst -> 'a) -> (unit -> 'a Free.t) -> 'a

     val run_and_unwrap: ('a dst -> 'a) -> (unit -> 'a result Free.t) -> 'a

    (**
      Semicolon operator

      If the expression in the left is successful, ignore it and return the expression in the right.
      You can only use this operator if the expression in the left evaluates to "unit result".
      If you want to discard a value different than unit, is the (>>>).
    *)
    val (>>): unit promise -> 'b promise -> 'b promise

    (**
      Discard operator

      If the expression in the left is successful, ignore it and return
      the expression in the right.
    *)
    val (>>>): 'a promise -> 'b promise -> 'b promise
  end


  (**
    Minimum specification for a library.

    It contains:
     - A functor `Algebra` describing an algebra
     - A module `S` with useful interfaces for this algebra
     - A context translator, called `Proxy`, to help other DSL writers
       integrate with this library.

    This defines a base for all meta programming around our DSLs.
    This is why FreeMonads are easy to use on OCaml.

    Note: This spec shouldn't be written manually. It can be derived
    from any functor describing an algebra. This is why we have a SpecFor(...)
    module.
  *)
  module type Spec = sig
    module Algebra : Functor

    module S : sig
      module type Natural = sig
        type 'a src
        val apply: 'a src -> 'a Algebra.t
      end

      module type Context = Context
        with type 'a src = 'a Algebra.t

      module type Runner = sig
        val run: 'a Algebra.t -> 'a
        val debug: 'a Algebra.t -> 'a
      end
    end

    module Proxy : functor(T:S.Natural) -> sig
      module For : functor(Ctx:S.Context) -> sig
        include Context with
          type 'a dst = 'a Ctx.dst
          and module Free = Ctx.Free
          and type 'a free  = 'a Ctx.Free.t
          and type 'a src  = 'a T.src
      end
    end
  end (* Machine.Spec *)

  (**
    This signature just summarizes the common requirements to build a DSL library.
  *)
  module type Library = sig
    module Errors : Errors
    exception Error of Errors.t

    module Algebra : Functor
    module Spec : Spec with module Algebra = Algebra
  end

  (**
    A slightly modified version of the Library signature.

    This is used to close a library around a default interpreter.
    The interpreter must have two methods:
     - run: default interpreter
     - debug: higher verbosity interpreter

    The advantage of this definition is to use the Assemble builder:
     - Better version of the Combine builder for algebras
     - Merge interpreters
     - Produce a new combined Runtime
  *)
  module type Runtime = sig
    module Algebra : Functor
    module Spec : Spec with module Algebra = Algebra
    module Runner : sig
      val run : 'a Algebra.t -> 'a
      val debug : 'a Algebra.t -> 'a
    end
  end

  (**
    The signature for the Assemble builder

    Check the Runtime definition for more information.
  *)
  module type Assembly = functor (R1:Runtime) (R2:Runtime) -> Runtime
end

open S


(**
 This builder enforces the conventions around the definitions of
 DSLs errors.

 Check the definition of ErrorForSpec for more information.
*)
module ErrorFor(E:Errors) : ErrorForSpec
  with type error := E.t
 = struct
  type error = E.t

  exception Error of error

  let string_of_error (e:error) : string =
    Sexplib.Sexp.to_string_hum @@ E.sexp_of_t e

  let rollback f e =
    f (Prelude.CoreResult.throw (Error e))
end

(**
  Automates the generation of minimum specification
  for a library based on its algebra.
*)
module SpecFor(L:Functor) : Spec
  with module Algebra = L
 = struct

  module Algebra = L

  module S = struct
    module type Context = Context
      with type 'a src = 'a L.t

    module type Natural = sig
      type 'a src
      val apply: 'a src -> 'a L.t
    end

    module type Runner = sig
      val run: 'a Algebra.t -> 'a
      val debug: 'a Algebra.t -> 'a
    end
  end

  module Proxy(T:S.Natural) = struct
    module For(Ctx:S.Context) : Context
      with type 'a dst = 'a Ctx.dst
       and module Free = Ctx.Free
       and type 'a free = 'a Ctx.Free.t
       and type 'a src = 'a T.src =
    struct
      type 'a src  = 'a T.src
      type 'a dst = 'a Ctx.dst

      type 'a free   = 'a Ctx.Free.t

      let apply v = Ctx.apply (T.apply v)

      let lift f = Ctx.Free.lift (apply f)

      module Free = Ctx.Free

      include Prelude.CoreResult.For (Ctx.Free)

      let (>>) x y = bind_if x (fun () -> y)
      let (>>>) x y = bind_if x (fun _ -> y)

      let run = Ctx.run
      let run_and_unwrap = Ctx.run_and_unwrap
    end  (* SpecFor.Proxy.For *)
  end (* SpecFor.Proxy *)

end (* SpecFor *)

module Library (Spec:Spec) (Errors:Errors) = struct
  exception Error of Errors.t

  let string_of_error (e:Errors.t) : string =
    Sexplib.Sexp.to_string_hum @@ Errors.sexp_of_t e

  let rollback f e =
    f (Prelude.CoreResult.throw (Error e))

  module type Partials = sig
    exception Error of Errors.t

    val string_of_error: Errors.t -> string

    module Context: Spec.S.Context

    (* type 'a promise *)
    type 'a result = 'a Context.promise
  end

  module Init (C:Spec.S.Context) : Partials
    with module Context = C
    and type 'a result = 'a C.promise
    =
  struct
    module Context = C

    let string_of_error = string_of_error
    type exn += Error = Error

    type 'a result = 'a Context.promise
  end
end


(**
  Combine two algebras.

  This builder generates these new modules:
   - Algebra: combined Algebra
   - Spec: specification for the combined Algebra
   - Natural: agregates all natural transformations (including context proxies)
*)
module Combine (L1:Functor) (L2:Functor) = struct

  module Algebra = struct
    type 'a t =
      | Case1 of 'a L1.t
      | Case2 of 'a L2.t

    let map f = function
      | Case1 v -> Case1 (L1.map f v)
      | Case2 v -> Case2 (L2.map f v)
  end

  module Spec = SpecFor (Algebra)

  module Natural = struct
    open Algebra
    open Spec

    let apply1 v = Case1 v
    let apply2 v = Case2 v

    module Proxy1 = Proxy(struct type 'a src = 'a L1.t let apply = apply1 end)
    module Proxy2 = Proxy(struct type 'a src = 'a L2.t let apply = apply2 end)
  end

end (* Combine *)

(**
  Combine 3 algebras

  Similar to the Combine builder
*)
module Combine3 (L1:Functor) (L2:Functor) (L3:Functor) = struct
  module Algebra = struct
    type 'a t =
      | Case1 of 'a L1.t
      | Case2 of 'a L2.t
      | Case3 of 'a L3.t

    let map f = function
      | Case1 v -> Case1 (L1.map f v)
      | Case2 v -> Case2 (L2.map f v)
      | Case3 v -> Case3 (L3.map f v)
  end

  module Spec = SpecFor(Algebra)

  module Natural = struct
    open Algebra
    open Spec

    let apply1 v = Case1 v
    let apply2 v = Case2 v
    let apply3 v = Case3 v

    module Proxy1 = Proxy(struct type 'a src = 'a L1.t let apply = apply1 end)
    module Proxy2 = Proxy(struct type 'a src = 'a L2.t let apply = apply2 end)
    module Proxy3 = Proxy(struct type 'a src = 'a L3.t let apply = apply3 end)
  end
end (* Combine3 *)



(**
  Combine 4 languages.

  Similar to the Combine builder
*)
module Combine4 (L1:Functor) (L2:Functor)
                (L3:Functor) (L4:Functor) =
struct
  module Algebra_A = Combine (L1) (L2)
  module Algebra_B = Combine (L3) (L4)

  module R = Combine (Algebra_A.Algebra) (Algebra_B.Algebra)

  module Algebra = R.Algebra
  module Spec = R.Spec

  module Natural = struct
    open Spec

    let apply1 v = R.Natural.apply1 (Algebra_A.Natural.apply1 v)
    let apply2 v = R.Natural.apply1 (Algebra_A.Natural.apply2 v)
    let apply3 v = R.Natural.apply2 (Algebra_B.Natural.apply1 v)
    let apply4 v = R.Natural.apply2 (Algebra_B.Natural.apply2 v)

    module Proxy1 = Proxy(struct type 'a src = 'a L1.t let apply = apply1 end)
    module Proxy2 = Proxy(struct type 'a src = 'a L2.t let apply = apply2 end)
    module Proxy3 = Proxy(struct type 'a src = 'a L3.t let apply = apply3 end)
    module Proxy4 = Proxy(struct type 'a src = 'a L4.t let apply = apply4 end)
  end
end (* Combine4 *)

(**
  Read the signature for Context for more information.
*)
module ContextFor(L:Functor)
  : Context
  with type 'a src = 'a L.t
   and type 'a dst = 'a L.t
= struct
  module Free = MakeFree (L)
  type 'a free   = 'a Free.t

  type 'a src = 'a L.t
  type 'a dst = 'a L.t
  let apply v = v

  let return f = Free.return f
  let lift f = Free.lift (apply f)

  include Prelude.CoreResult.For (Free)

  let run_and_unwrap runner program =
    Free.iter runner (unwrap_or raise (program ()))

  let run runner program =
    Free.iter runner (program ())

  let (>>) x y = bind_if x (fun () -> y)
  let (>>>) x y = bind_if x (fun _ -> y)
end

(**
  Virtually the same as the ContextFor builder, but for runtimes
*)
module ContextForRuntime(R:Runtime) = struct
  include ContextFor(R.Algebra)
end

(**
  Merge two runtimes.

  Check the Runtime definition for more information.
*)
module Assemble (R1:Runtime) (R2:Runtime) = struct
  include Combine (R1.Algebra) (R2.Algebra)

  module Runner : Spec.S.Runner = struct
    open Algebra

    let run = function
      | Case1 v -> R1.Runner.run v
      | Case2 v -> R2.Runner.run v

    let debug = function
      | Case1 v -> R1.Runner.debug v
      | Case2 v -> R2.Runner.debug v
  end
end

let _ =
  (module Assemble: Assembly)
