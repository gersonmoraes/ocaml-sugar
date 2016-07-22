module LwtMonad: Sugar_s.Monad = struct
  type 'a monad = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
end


module Result = struct
  open Sugar_result

  module Make(UserError:Sugar_s.Error): Sugar_monadic.Monadic_result_s
    with type error := UserError.error
     and type 'a monad := 'a LwtMonad.monad
  = struct
      (* type error = UserError.error
      type 'a monad = 'a LwtMonad.monad *)

      include (Sugar_monadic.Make (UserError) (LwtMonad))

    (* module GeneratedResult: Sugar_monadic.Monadic_result_s
      with type error := Error.error = struct

    end *)

  end
end
