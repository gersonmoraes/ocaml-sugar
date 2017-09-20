module Promise = Promise
module Result = Sugar_result
module Option = Option

module Strict = struct
  module Result = Strict_result
  module Promise = Strict_promise
end
