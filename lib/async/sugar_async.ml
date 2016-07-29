open Async.Std

module Monad = struct
  type 'a monad = 'a Deferred.t
  let return = Deferred.return
  let (>>=) = Deferred.bind
end

module Result = struct
  module Make(UserError:Sugar.Types.Error) = struct
    include Sugar.Promise.Make (Monad) (UserError)
  end
end
