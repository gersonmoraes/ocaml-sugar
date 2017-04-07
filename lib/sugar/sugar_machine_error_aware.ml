(* module Generic = Sugar_generic *)
open Sugar_generic

module Utils = struct
  let id = fun x -> x
  (* let ($) = (@@) *)
  let (%) = (@@)

  (** Function composition *)
  let (@) f g = fun v -> f (g v)

  (* this is an alias for function composition *)
  (* let (%) = (@) *)

  (* let ($) f a = (@@) *)

  (* type ('f, 'args) continuation = 'args -> 'f *)

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

  let is_some = function
    | Some _ -> true
    | None -> false
end

open Utils

module type Language = sig
  include Functor

  (* module Error : Sugar_types.Error *)

  (* type error *)

  module Result : Sugar_types.Result
    (* with type error := error *)

  (* module Result : Sugar_types.Result *)
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

module type NaturalError = sig
  type src
  type dst

  val apply: src -> dst
  val reverse: dst -> src option
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

  (* an interesting place to *)
  val return : 'a -> 'a Free.t
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
  module Core : Language

  module S : sig
    module type Natural = sig
      type 'a src
      val apply: 'a src -> 'a Core.t
    end

    module type Error = sig
      type dst
      val apply: Core.Result.error -> dst
      val reverse: dst -> Core.Result.error option
    end
    (* module type Error = NaturalError
      with type src = Core.Result.error *)

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


(**
  Automates the generation of minimum specification
  for a library based on its algebra.
*)
module SpecFor(L:Language) : Spec
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

    module type Error = sig
      type dst
      val apply: Core.Result.error -> dst
      val reverse: dst -> Core.Result.error option
    end

    (* module type Error = NaturalError
      with type src = Core.Result.error *)

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
      let return f = Ctx.Free.return f

      module Free = Ctx.Free
    end  (* Spec.Proxy.For *)
  end (* Spec.Proxy *)

end (* SpecFor *)

module Combine (F1:Language) (F2:Language) = struct

  module Core = struct
    type 'a t =
      | Case1 of 'a F1.t
      | Case2 of 'a F2.t

    let map f = function
      | Case1 v -> Case1 (F1.map f v)
      | Case2 v -> Case2 (F2.map f v)

    module Result = Sugar.MakeResult (struct
      type error
        = Error1 of F1.Result.error
        | Error2 of F2.Result.error
    end)
  end
  module Spec = SpecFor (Core)

  open Core
  open Spec

  module Natural = struct
    let apply1 v = Case1 v
    let apply2 v = Case2 v

    module Proxy1 = Proxy(struct type 'a src = 'a F1.t let apply = apply1 end)
    module Proxy2 = Proxy(struct type 'a src = 'a F2.t let apply = apply2 end)
  end
end (* Combine *)

module Combine3 (F1:Language) (F2:Language) (F3:Language) = struct

  module Core = struct
    type 'a t =
      | Case1 of 'a F1.t
      | Case2 of 'a F2.t
      | Case3 of 'a F3.t

    let map f = function
      | Case1 v -> Case1 (F1.map f v)
      | Case2 v -> Case2 (F2.map f v)
      | Case3 v -> Case3 (F3.map f v)

    module Result = Sugar.MakeResult (struct
      type error
        = Error1 of F1.Result.error
        | Error2 of F2.Result.error
        | Error3 of F3.Result.error
    end)
  end
  module Spec = SpecFor(Core)

  open Core
  open Spec

  module Natural = struct
    let apply1 v = Case1 v
    let apply2 v = Case2 v
    let apply3 v = Case3 v

    module Proxy1 = Proxy(struct type 'a src = 'a F1.t let apply = apply1 end)
    module Proxy2 = Proxy(struct type 'a src = 'a F2.t let apply = apply2 end)
    module Proxy3 = Proxy(struct type 'a src = 'a F3.t let apply = apply3 end)
  end

end (* Combine3 *)



(**
  Combine 4 languages.
*)
module Combine4 (F1:Language) (F2:Language)
                (F3:Language) (F4:Language) =
struct
  module Core1_2 = Combine (F1) (F2)
  module Core3_4 = Combine (F3) (F4)
  module R = Combine (Core1_2.Core) (Core3_4.Core)

  module Core = R.Core
  module Spec = R.Spec

  open Spec

  module Natural = struct
    let apply1 v = R.Natural.apply1 (Core1_2.Natural.apply1 v)
    let apply2 v = R.Natural.apply1 (Core1_2.Natural.apply2 v)
    let apply3 v = R.Natural.apply2 (Core3_4.Natural.apply1 v)
    let apply4 v = R.Natural.apply2 (Core3_4.Natural.apply2 v)

    module Proxy1 = Proxy(struct type 'a src = 'a F1.t let apply = apply1 end)
    module Proxy2 = Proxy(struct type 'a src = 'a F2.t let apply = apply2 end)
    module Proxy3 = Proxy(struct type 'a src = 'a F3.t let apply = apply3 end)
    module Proxy4 = Proxy(struct type 'a src = 'a F4.t let apply = apply4 end)
  end
end (* Combine4 *)

module type Runtime = sig
  module Core : Language
  module Spec : Spec with module Core = Core
  module Runner : sig
    val run : 'a Core.t -> 'a
    val debug : 'a Core.t -> 'a
  end
end

module ForLanguage(L:Language) = struct
  module Free = MakeFree (L)
  type 'a free   = 'a Free.t
  type 'a free_f = 'a Free.f

  (* TODO: not used *)
  type 'a t = 'a L.t
  type 'a src = 'a L.t
  type 'a dst = 'a t
  let apply = id

  (* TODO:
      The implementation of this function may change, to better
      integrate with error aware computations. Or not.
  *)
  let return f = Free.return f
  let lift f = Free.lift (apply f)
  let run runner program =
    Free.iter runner program
end

module For(R:Runtime) = struct
  include ForLanguage(R.Core)
end

module type Assembly = functor (R1:Runtime) (R2:Runtime) -> Runtime

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

(*
let (++) r1 r2 =
  let module R1 = (val r1:Runtime) in
  let module R2 = (val r2:Runtime) in
  let module R3 = Assemble (R1) (R2) in
  (module R3: Runtime)
*)




let _ =
  (module Assemble: Assembly)
