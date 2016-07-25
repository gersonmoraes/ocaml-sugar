open Async.Std

module Result = struct
  module Make(UserError:Sugar.Types.Error) = struct

    include Sugar.Promise.Make
      (struct
        type 'a monad = 'a Deferred.t
        let return = Deferred.return
        let (>>=) = Deferred.bind

        let semicolon x y =
          Deferred.both x y
          >>= fun (_x, y) ->
          return y
      end)
      (UserError)

  end
end
