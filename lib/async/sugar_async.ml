open Async.Std

module Monad = struct
  type 'a monad = 'a Deferred.t
  let return = Deferred.return
  let (>>=) = Deferred.bind
end

module MakeResult(UserError:Sugar.Types.Error) = struct
  include Sugar.MakePromise (Monad) (UserError)
end
