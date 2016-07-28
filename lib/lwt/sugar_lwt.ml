
module Result = struct

  module Make(UserError:Sugar.Types.Error) = struct
    include Sugar.Promise.Make
      (struct
        type 'a monad = 'a Lwt.t
        let return = Lwt.return
        let (>>=) = Lwt.bind
      end)
      (UserError)
  end

end
