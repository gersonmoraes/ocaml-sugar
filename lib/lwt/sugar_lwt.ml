module Monad: Sugar_s.Monad = struct
  type 'a monad = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
end


module Result = struct
  open Sugar_result

  module Make(UserError:Sugar_s.Error) (*: Sugar_monadic.Monadic_result_s*)
    (* with error = Error.error *)
  = struct

    (* module GeneratedResult: Sugar_monadic.Monadic_result_s
      with type error := Error.error = struct
       include (Sugar_monadic.Make (Error) (Monad))
    end *)

  end
end
