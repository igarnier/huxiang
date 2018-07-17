module Nothing =
struct

  type t = unit
  [@@deriving eq, yojson, show]

end

module ClientToBroker =
struct
  
  type t =
    | Payement of int
  [@@deriving eq, yojson, show]

end

module BrokerToService =
struct
  
  type t =
    | BulkPayement of int
  [@@deriving eq, yojson, show]

end
