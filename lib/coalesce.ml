module type Params = Product.Params

module Make(P : Params)(S : Process.Scheduler)(L : Leadership.S)(C : Crypto.Credentials) =
(struct

  module Prod = Product.Make(P)

  module RepProd = Replica.Make(Prod)(S)(L)(P)(C)

  include RepProd
end : NetProcess.S)
