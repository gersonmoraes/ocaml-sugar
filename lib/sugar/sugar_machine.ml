module Generic = Sugar_generic

module Utils = struct
  let id = fun x -> x
  let ($) = (@@)
  let (%) = (@@)
  let (@) f g = fun v -> f (g v)
  type 'f msg = string -> 'f

  type ('arg, 'f) next = 'arg -> 'f


  (* type 'f unrelated = unit -> 'f *)
  type 'f unrelated = (unit, 'f) next

  (* type ('a, 'f) then_unrelated = 'a -> unit -> 'f *)

  let is_some = function
    | Some _ -> true
    | None -> false
end

open Utils

(**
  A language is just a functor
*)
module type Language = Generic.Functor

(**
  Translation is an injective operation between two languages.
*)
module type Translation = sig
  type 'a src
  type 'a dest
  val translate: 'a src -> 'a dest
end


module Id(L:Language) : Translation
  with type 'a src = 'a L.t
   and type 'a dest = 'a L.t
=
struct
  type 'a t = 'a L.t
  type 'a src = 'a L.t
  type 'a dest = 'a t

  let translate = id
end




(* TODO: this interface lacks an Infix module *)
module type Context = sig
  module Free : Generic.FreeMonad

  type 'a free   = 'a Free.t
  type 'a free_f = 'a Free.f

  include Translation
    with type 'a dest = 'a free_f

  val lift: 'a src -> 'a Free.t
  val return : 'a -> 'a Free.t
end



module SinglePlayer(L:Language) = struct
  module Free = Generic.Free (L)
  type 'a free   = 'a Free.t
  type 'a free_f = 'a Free.f

  include Id(L)

  let return f = Free.return f
  let lift f = Free.lift (translate f)
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

  let lift f = Free.lift (translate f)
  let return f = Free.return f
end *)

module type Spec = sig
  module Core : Language

  module S : sig
    module type FromMe = Translation
      with type 'a dest = 'a Core.t

    module type Context = Context
      with type 'a src = 'a Core.t

    module type ToMe = sig
      type 'a src
      val translate: 'a src -> 'a Core.t
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

module SpecFor(L:Language) : Spec
  with module Core = L
 = struct

  module Core = L

  module S = struct

    module type FromMe = Translation
      with type 'a dest = 'a L.t

    module type Context = Context
      with type 'a src = 'a L.t

    module type ToMe = sig
      type 'a src
      val translate: 'a src -> 'a L.t
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

      let translate v = Ctx.translate (T.translate v)

      let lift f = Ctx.Free.lift (translate f)
      let return f = Ctx.Free.return f

      module Free = Ctx.Free
    end  (* end of Spec.Proxy.For *)
  end (* end of Spec.Proxy *)

end (* end of SpecFor *)

module Combine (L1:Language) (L2:Language) = struct

  module Core = struct
    type 'a tree =
      | Branch1 of 'a L1.t
      | Branch2 of 'a L2.t

    type 'a t = 'a tree

    let map f = function
      | Branch1 v -> Branch1 (L1.map f v)
      | Branch2 v -> Branch2 (L2.map f v)
  end
  open Core

  let lift_branch1 v = Branch1 v
  let lift_branch2 v = Branch2 v

  module Spec = SpecFor
    (struct
      type 'a t = 'a tree
      let map = map
    end)

  module T1 = Spec.Proxy
    (struct
      type 'a src = 'a L1.t
      let translate = lift_branch1
    end)

  module T2 = Spec.Proxy
    (struct
      type 'a src = 'a L2.t
      let translate = lift_branch2
    end)
end (* end of Machine.Combine *)

module Combine3 (L1:Language) (L2:Language) (L3:Language) = struct
  module Core = struct
    type 'a tree =
      | Branch1 of 'a L1.t
      | Branch2 of 'a L2.t
      | Branch3 of 'a L3.t

    type 'a t = 'a tree

    let map f = function
      | Branch1 v -> Branch1 (L1.map f v)
      | Branch2 v -> Branch2 (L2.map f v)
      | Branch3 v -> Branch3 (L3.map f v)
  end
  open Core

  let lift_branch1 v = Branch1 v
  let lift_branch2 v = Branch2 v
  let lift_branch3 v = Branch3 v

  module Spec = SpecFor(Core)

  module T1 = Spec.Proxy
    (struct
      type 'a src = 'a L1.t
      let translate = lift_branch1
    end)

  module T2 = Spec.Proxy
    (struct
      type 'a src = 'a L2.t
      let translate = lift_branch2
    end)

  module T3 = Spec.Proxy
    (struct
      type 'a src = 'a L3.t
      let translate = lift_branch3
    end)
end (* end of Machine.Combine3 *)



(* TODO:
    - This is incomplete, since we created to seperate languages,
      Core1 and Core2. The resulting translators would not be compatible to
      L1, L2, L3 and L4.
    - Should we try to combine them again?
*)
module Combine4 (L1:Language) (L2:Language)
                (L3:Language) (L4:Language) =
struct
  module Core1_2 = Combine (L1) (L2)
  module Core3_4 = Combine (L3) (L4)

  module R = Combine (Core1_2.Core) (Core3_4.Core)

  module Core = R.Core
  module Spec = R.Spec

  module T1 = Spec.Proxy
    (struct
      type 'a src = 'a L1.t
      let translate v = R.lift_branch1 (Core1_2.lift_branch1 v)
    end)

  module T2 = Spec.Proxy
    (struct
      type 'a src = 'a L2.t
      let translate v = R.lift_branch1 (Core1_2.lift_branch2 v)
    end)

  module T3 = Spec.Proxy
    (struct
      type 'a src = 'a L3.t
      let translate v = R.lift_branch2 (Core3_4.lift_branch1 v)
    end)

  module T4 = Spec.Proxy
    (struct
      type 'a src = 'a L4.t
      let translate v = R.lift_branch2 (Core3_4.lift_branch2 v)
    end)
end (* end of Combine4 *)


module type Runner = sig
  module Core : Language

  val run : 'a Core.t -> 'a
  val debug : 'a Core.t -> 'a
end

module type Runtime = sig
  module Core : Language
  module Spec : Spec with module Core = Core
  module Runner : Runner with module Core = Core
end

module For(R:Runtime) = struct
  include SinglePlayer(R.Core)
end

module type Assembly = functor (R1:Runtime) (R2:Runtime) -> Runtime

module Assemble (R1:Runtime) (R2:Runtime) = struct
  module Merge = Combine (R1.Core) (R2.Core)

  module Core = Merge.Core
  module Spec = SpecFor(Core)
  module T1 = Merge.T1
  module T2 = Merge.T2

  module Runner = struct
    module Core = Core

    let run = function
      | Core.Branch1 v -> R1.Runner.run v
      | Core.Branch2 v -> R2.Runner.run v

    let debug = function
      | Core.Branch1 v -> R1.Runner.debug v
      | Core.Branch2 v -> R2.Runner.debug v
  end
end

(*
let (++) r1 r2 =
  let module R1 = (val r1:Runtime) in
  let module R2 = (val r2:Runtime) in
  let module R3 = Assemble (R1) (R2) in
  (module R3: Runtime) *)

let _ =
  (module Assemble: Assembly)
