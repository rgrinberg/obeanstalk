include Beanstalk

module Stringly = struct
  module Worker = Beanstalk.Worker(struct
    type t = string
    let serialize x = x
    let deserialize x = x
    let size = String.length
  end)
  (*module Worker : module type of W with type s := string = W*)
  module Job = Worker.Job
end
