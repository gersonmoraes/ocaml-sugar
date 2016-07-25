
module Result = struct

  module Make(UserError:Sugar.Types.Error) = struct

    include Sugar.Promise.Make
      (struct
        type 'a monad = 'a Lwt.t
        let return = Lwt.return
        let (>>=) = Lwt.bind

        let semicolon m1 m2 =
          lwt _x = m1 and y = m2 in
          return y
      end)
      (UserError)

    (* include Sugar_promise.Make (LwtMonad) (UserError) *)
  end

end
