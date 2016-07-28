open Async.Std

module Result = struct

  module Make(UserError:Sugar.Types.Error) = struct
    include Sugar.Promise.Make
      (struct
        type 'a monad = 'a Deferred.t
        let return = Deferred.return
        let (>>=) = Deferred.bind
      end)
      (UserError)
  end

end
