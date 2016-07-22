module LwtMonad: Sugar_s.Monad
  with type 'a monad := 'a Lwt.t =
struct
  type 'a monad = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
end


module Result = struct

  module Make(UserError:Sugar_s.Error) = struct

    include Sugar.Std.Monadic.Make
      (struct
        type 'a monad = 'a Lwt.t
        let return = Lwt.return
        let (>>=) = Lwt.bind
      end)
      (UserError)

    (* include Sugar_monadic.Make (LwtMonad) (UserError) *)
  end

end
