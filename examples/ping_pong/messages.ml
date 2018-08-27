open Bin_prot
open Huxiang

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
