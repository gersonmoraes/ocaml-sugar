
(* Type signatures for common modules *)
module Types = Sugar_types

(** Create a result module for a project *)
module Result = Sugar_result

(** Implements a monadic interface for wraped asynchronous results *)
module Promise = Sugar_promise


(** Opaque error results *)
module Opaque = struct

  (** This module implements the Types.Result using the standard option type *)
  module Option = Sugar_option

  (** An empty error module *)
  module OpaqueError = struct
      type error = unit
  end
  (** This module implements the Types.Result using the standard option type *)
  module Result = Sugar_result.Make(OpaqueError)
end
