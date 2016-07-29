
module Monad = struct
  type 'a monad = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
end

module Result = struct
  module Make(UserError:Sugar.Types.Error) = struct
    include Sugar.Promise.Make (Monad) (UserError)
  end
end
