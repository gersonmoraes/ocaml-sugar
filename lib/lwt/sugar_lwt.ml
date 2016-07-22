module Monad: Sugar_s.Monad = struct
  type 'a m = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
end


module Result = struct

  open Sugar_result

  module Make(UserError:Sugar_result.Error): Sugar_monadic.S
    with type error := UserError.error
      and type 'a monad := 'a Lwt.t
      and type 'a result := 'a result Lwt.t =
  struct

    (* type 'a result = ('a, UserError.error) generic_result monad *)

  end


end
