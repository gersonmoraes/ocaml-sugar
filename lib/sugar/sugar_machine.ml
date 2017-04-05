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

(**
  A language is just a functor
*)
(* module type Language = Generic.Functor *)

(**
  Natural Transformation.
  It represents a translation from a functor to another.
*)
module type Natural = sig
  type 'a src
  type 'a dest
  val apply: 'a src -> 'a dest
end

module Id(L:Functor) : Natural
  with type 'a src = 'a L.t
   and type 'a dest = 'a L.t
=
struct
  type 'a t = 'a L.t
  type 'a src = 'a L.t
  type 'a dest = 'a t

  let apply = id
end




(* TODO: this interface lacks an Infix module *)
module type Context = sig
  module Free : FreeMonad

  type 'a free   = 'a Free.t
  type 'a free_f = 'a Free.f

  include Natural
    with type 'a dest = 'a free_f

  val lift: 'a src -> 'a Free.t
  val return : 'a -> 'a Free.t
end



(* This is probably broken *)
(* module ContextFor2(L:Language) : Context
  with type 'a src = 'a L.t
  =
struct
  include Id(L)
  module Free = Generic.Free (L)

  type 'a free_f = 'a Free.f
  type 'a free = 'a Free.t

  let lift f = Free.lift (apply f)
  let return f = Free.return f
end *)

module type Spec = sig
  module Core : Functor

  module S : sig
    module type FromMe = Natural
      with type 'a dest = 'a Core.t

    module type Context = Context
      with type 'a src = 'a Core.t

    module type ToMe = sig
      type 'a src
      val apply: 'a src -> 'a Core.t
    end

    module type Runner = sig
      val run: 'a Core.t -> 'a
      val debug: 'a Core.t -> 'a
    end

  end  (* end of Specification.S *)

  module Proxy : functor(T:S.ToMe) -> sig
    module For : functor(Ctx:S.Context) -> sig
      include Context with
        module Free = Ctx.Free
        and type 'a free  = 'a Ctx.Free.t
        and type 'a free_f = 'a Ctx.Free.f
        and type 'a src  = 'a T.src
        and type 'a dest = 'a Ctx.Free.f
    end
  end
end (* end of Machine.Spec *)

module SpecFor(L:Functor) : Spec
  with module Core = L
 = struct

  module Core = L

  module S = struct

    module type FromMe = Natural
      with type 'a dest = 'a L.t

    module type Context = Context
      with type 'a src = 'a L.t

    module type ToMe = sig
      type 'a src
      val apply: 'a src -> 'a L.t
    end


    module type Runner = sig
      (* module Core = Core *)
      val run: 'a Core.t -> 'a
      val debug: 'a Core.t -> 'a
    end
  end (* end of SpecFor.S *)

  module Proxy(T:S.ToMe) = struct
    module For(Ctx:S.Context) : Context
     with module Free = Ctx.Free
     with type 'a free  = 'a Ctx.Free.t
      and type 'a free_f = 'a Ctx.Free.f
      and type 'a src  = 'a T.src
      and type 'a dest = 'a Ctx.Free.f

    =
    struct
      type 'a src  = 'a T.src
      type 'a dest = 'a Ctx.dest

      type 'a free   = 'a Ctx.Free.t
      type 'a free_f = 'a Ctx.Free.f

      let apply v = Ctx.apply (T.apply v)

      let lift f = Ctx.Free.lift (apply f)
      let return f = Ctx.Free.return f

      module Free = Ctx.Free
    end  (* end of Spec.Proxy.For *)
  end (* end of Spec.Proxy *)

end (* end of SpecFor *)

module Combine (F1:Functor) (F2:Functor) = struct

  module Core = struct
    type 'a t =
      | Case1 of 'a F1.t
      | Case2 of 'a F2.t

    let map f = function
      | Case1 v -> Case1 (F1.map f v)
      | Case2 v -> Case2 (F2.map f v)
  end
  open Core

  let return1 v = Case1 v
  let return2 v = Case2 v

  module Spec = SpecFor (Core)

  module T1 = Spec.Proxy
    (struct
      type 'a src = 'a F1.t
      let apply = return1
    end)

  module T2 = Spec.Proxy
    (struct
      type 'a src = 'a F2.t
      let apply = return2
    end)
end (* end of Machine.Combine *)

module Combine3 (F1:Functor) (F2:Functor) (F3:Functor) = struct
  module Core = struct
    type 'a t =
      | Case1 of 'a F1.t
      | Case2 of 'a F2.t
      | Case3 of 'a F3.t

    let map f = function
      | Case1 v -> Case1 (F1.map f v)
      | Case2 v -> Case2 (F2.map f v)
      | Case3 v -> Case3 (F3.map f v)
  end
  open Core

  let return1 v = Case1 v
  let return2 v = Case2 v
  let return3 v = Case3 v

  module Spec = SpecFor(Core)

  module T1 = Spec.Proxy
    (struct
      type 'a src = 'a F1.t
      let apply = return1
    end)

  module T2 = Spec.Proxy
    (struct
      type 'a src = 'a F2.t
      let apply = return2
    end)

  module T3 = Spec.Proxy
    (struct
      type 'a src = 'a F3.t
      let apply = return3
    end)
end (* end of Machine.Combine3 *)



(**
  Combine 4 languages.


*)
module Combine4 (F1:Functor) (F2:Functor)
                (F3:Functor) (F4:Functor) =
struct
  module Core1_2 = Combine (F1) (F2)
  module Core3_4 = Combine (F3) (F4)

  module R = Combine (Core1_2.Core) (Core3_4.Core)

  module Core = R.Core
  module Spec = R.Spec

  module T1 = Spec.Proxy
    (struct
      type 'a src = 'a F1.t
      let apply v = R.return1 (Core1_2.return1 v)
    end)

  module T2 = Spec.Proxy
    (struct
      type 'a src = 'a F2.t
      let apply v = R.return1 (Core1_2.return2 v)
    end)

  module T3 = Spec.Proxy
    (struct
      type 'a src = 'a F3.t
      let apply v = R.return2 (Core3_4.return1 v)
    end)

  module T4 = Spec.Proxy
    (struct
      type 'a src = 'a F4.t
      let apply v = R.return2 (Core3_4.return2 v)
    end)
end (* end of Combine4 *)

module type Runtime = sig
  module Core : Functor
  module Spec : Spec with module Core = Core
  module Runner : sig
    val run : 'a Core.t -> 'a
    val debug : 'a Core.t -> 'a
  end
end

module ForLanguage(L:Functor) = struct
  module Free = MakeFree (L)
  type 'a free   = 'a Free.t
  type 'a free_f = 'a Free.f

  include Id(L)

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
  module Merge = Combine (R1.Core) (R2.Core)

  module Core = Merge.Core
  module Spec = SpecFor(Core)
  module T1 = Merge.T1
  module T2 = Merge.T2

  module Runner : Spec.S.Runner = struct
    let run = function
      | Core.Case1 v -> R1.Runner.run v
      | Core.Case2 v -> R2.Runner.run v

    let debug = function
      | Core.Case1 v -> R1.Runner.debug v
      | Core.Case2 v -> R2.Runner.debug v
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
