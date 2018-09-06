open Bin_prot

open Std

module PingMsg =
struct
  
  type t =
    | Ping of int
  [@@deriving eq, show, bin_io]

end

module PongMsg =
struct

  type t =
    | Pong of int
  [@@deriving eq, show, bin_io]
  
end

module Nothing =
struct

  type t = unit
  [@@deriving eq, show, bin_io]

end
