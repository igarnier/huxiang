module type Params = Product.Params

module Make(P : Params)(S : Process.Scheduler)(L : Leadership.S) =
(struct

  module Prod = Product.Make(P)

  module RepProd = Replica.Make(Prod)(S)(L)(P)

  include RepProd
end : NetProcess.S)
