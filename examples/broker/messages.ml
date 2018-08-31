open Bin_prot.Std

module Nothing =
struct

  type t = unit
  [@@deriving eq, show, bin_io]

end

module ClientToBroker =
struct
  
  type t =
    | Payement of int
  [@@deriving eq, show, bin_io]

end

module BrokerToService =
struct
  
  type t =
    | BulkPayement of int
  [@@deriving eq, show, bin_io]

end
