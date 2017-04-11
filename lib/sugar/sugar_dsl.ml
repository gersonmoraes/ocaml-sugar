(* module Generic = Sugar_generic *)
open Sugar_generic

module Error = struct
  type t = exn
end

module S = struct

  module type Result = Sugar_types.Result
    with type error = exn

  module type Errors = sig
    type t

    val t_of_sexp : Sexplib.Type.t -> t
    val sexp_of_t : t -> Sexplib.Type.t
  end

  module type ErrorForSpec = sig
    type error

    exception Error of error

    val string_of_error: error -> string
  end

  (**
    Natural Transformation.
    It represents a translation from a functor to another.
  *)
  module type Natural = sig
    type 'a src
    type 'a dst
    val apply: 'a src -> 'a dst
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
    module Free : FreeMonad

    type 'a free   = 'a Free.t
    type 'a free_f = 'a Free.f

    include Natural
      with type 'a dst = 'a free_f

    val lift: 'a src -> 'a Free.t

    (* TODO:
        - This instruction should be removed from here.
          It is not error aware. We need functionality like this in the result module.
    *)
    (* val return : 'a -> 'a Free.t *)

    (* This result is compatible to the free monad *)
    module Result : Sugar_types.Promise
      with type error := Error.t
       and type 'a monad := 'a Free.t
  end


  (**
    Minimum specification for a library.

    It contains:
     - A functor `Core` describing an algebra
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
    module Core : Functor

    module S : sig
      module type Natural = sig
        type 'a src
        val apply: 'a src -> 'a Core.t
      end

      module type Context = Context
        with type 'a src = 'a Core.t

      module type Runner = sig
        val run: 'a Core.t -> 'a
        val debug: 'a Core.t -> 'a
      end
    end

    module Proxy : functor(T:S.Natural) -> sig
      module For : functor(Ctx:S.Context) -> sig
        include Context with
          module Free = Ctx.Free
          and type 'a free  = 'a Ctx.Free.t
          and type 'a free_f = 'a Ctx.Free.f
          and type 'a src  = 'a T.src
          and type 'a dst = 'a Ctx.Free.f
      end
    end
  end (* Machine.Spec *)


  module type Library = sig
    module Errors : Errors
    exception Error of Errors.t

    module Core : Functor
    module Spec : Spec with module Core = Core
  end

  module type Runtime = sig
    module Core : Functor
    module Spec : Spec with module Core = Core
    module Runner : sig
      val run : 'a Core.t -> 'a
      val debug : 'a Core.t -> 'a
    end
  end

  module type Assembly = functor (R1:Runtime) (R2:Runtime) -> Runtime


end

open S


module Prelude = struct
  let id = fun x -> x

  (** Function composition *)
  let (@) f g = fun v -> f (g v)

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

module CoreResult = Sugar_result.Make (Error)

open Prelude

module ErrorFor(E:Errors) : ErrorForSpec
  with type error := E.t
 = struct
  type error = E.t

  exception Error of error

  let string_of_error (e:error) : string =
    Sexplib.Sexp.to_string_hum @@ E.sexp_of_t e
end

(**
  Automates the generation of minimum specification
  for a library based on its algebra.
*)
module SpecFor(L:Functor) : Spec
  with module Core = L
 = struct

  module Core = L

  module S = struct
    module type Context = Context
      with type 'a src = 'a L.t

    module type Natural = sig
      type 'a src
      val apply: 'a src -> 'a L.t
    end

    module type Runner = sig
      val run: 'a Core.t -> 'a
      val debug: 'a Core.t -> 'a
    end
  end (* SpecFor.S *)

  module Proxy(T:S.Natural) = struct
    module For(Ctx:S.Context) : Context
     with module Free = Ctx.Free
     with type 'a free  = 'a Ctx.Free.t
      and type 'a free_f = 'a Ctx.Free.f
      and type 'a src  = 'a T.src
      and type 'a dst = 'a Ctx.Free.f =
    struct
      type 'a src  = 'a T.src
      type 'a dst = 'a Ctx.dst

      type 'a free   = 'a Ctx.Free.t
      type 'a free_f = 'a Ctx.Free.f

      let apply v = Ctx.apply (T.apply v)

      let lift f = Ctx.Free.lift (apply f)

      module Free = Ctx.Free

      module Result = CoreResult.For (Ctx.Free)
    end  (* Spec.Proxy.For *)
  end (* Spec.Proxy *)

end (* SpecFor *)

module Combine (L1:Functor) (L2:Functor) = struct

  module Core = struct
    type 'a t =
      | Case1 of 'a L1.t
      | Case2 of 'a L2.t

    let map f = function
      | Case1 v -> Case1 (L1.map f v)
      | Case2 v -> Case2 (L2.map f v)
  end

  module Spec = SpecFor (Core)

  module Natural = struct
    open Core
    open Spec

    let apply1 v = Case1 v
    let apply2 v = Case2 v

    module Proxy1 = Proxy(struct type 'a src = 'a L1.t let apply = apply1 end)
    module Proxy2 = Proxy(struct type 'a src = 'a L2.t let apply = apply2 end)
  end

end (* Combine *)

module Combine3 (L1:Functor) (L2:Functor) (L3:Functor) = struct
  module Core = struct
    type 'a t =
      | Case1 of 'a L1.t
      | Case2 of 'a L2.t
      | Case3 of 'a L3.t

    let map f = function
      | Case1 v -> Case1 (L1.map f v)
      | Case2 v -> Case2 (L2.map f v)
      | Case3 v -> Case3 (L3.map f v)
  end

  module Spec = SpecFor(Core)

  module Natural = struct
    open Core
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
*)
module Combine4 (L1:Functor) (L2:Functor)
                (L3:Functor) (L4:Functor) =
struct
  module Core_A = Combine (L1) (L2)
  module Core_B = Combine (L3) (L4)

  module R = Combine (Core_A.Core) (Core_B.Core)

  module Core = R.Core
  module Spec = R.Spec

  module Natural = struct
    open Spec

    let apply1 v = R.Natural.apply1 (Core_A.Natural.apply1 v)
    let apply2 v = R.Natural.apply1 (Core_A.Natural.apply2 v)
    let apply3 v = R.Natural.apply2 (Core_B.Natural.apply1 v)
    let apply4 v = R.Natural.apply2 (Core_B.Natural.apply2 v)

    module Proxy1 = Proxy(struct type 'a src = 'a L1.t let apply = apply1 end)
    module Proxy2 = Proxy(struct type 'a src = 'a L2.t let apply = apply2 end)
    module Proxy3 = Proxy(struct type 'a src = 'a L3.t let apply = apply3 end)
    module Proxy4 = Proxy(struct type 'a src = 'a L4.t let apply = apply4 end)
  end
end (* Combine4 *)


module ContextFor(L:Functor) = struct
  module Free = MakeFree (L)
  type 'a free   = 'a Free.t
  type 'a free_f = 'a Free.f

  type 'a src = 'a L.t
  type 'a dst = 'a L.t
  let apply = id

  let return f = Free.return f
  let lift f = Free.lift (apply f)

  module Result = CoreResult.For (Free)

  let run_error_aware runner program =
    Free.iter runner (Result.unwrap_or raise (program ()))

  let run runner program =
    Free.iter runner (program ())
end

module ContextForRuntime(R:Runtime) = struct
  include ContextFor(R.Core)
end

module Assemble (R1:Runtime) (R2:Runtime) = struct
  include Combine (R1.Core) (R2.Core)

  module Runner : Spec.S.Runner = struct
    open Core

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
