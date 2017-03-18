
module Monad = struct
  type 'a monad = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
end

module MakeResult(UserError:Sugar.Types.Error) = struct
  include Sugar.MakePromise (Monad) (UserError)
end
