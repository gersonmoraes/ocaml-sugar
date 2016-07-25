module LwtMonad: Sugar_types.Monad
  with type 'a monad := 'a Lwt.t =
struct
  type 'a monad = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
end


module Result = struct

  module Make(UserError:Sugar.Std.Error) = struct

    include Sugar.Monadic.Make
      (struct
        type 'a monad = 'a Lwt.t
        let return = Lwt.return
        let (>>=) = Lwt.bind
      end)
      (UserError)

    (* include Sugar_monadic.Make (LwtMonad) (UserError) *)
  end

end
