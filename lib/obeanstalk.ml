include Beanstalk

module Stringly = struct
  module S = struct
    type t = string
    let serialize x   = x
    let deserialize x = x
    let size          = String.length
  end
  module Worker = Beanstalk.Worker(S)
end
