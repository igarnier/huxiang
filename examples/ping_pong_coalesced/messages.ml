module PingMsg =
struct
  
  type t =
    | Ping of int
  [@@deriving eq, yojson, show]

end

module PongMsg =
struct

  type t =
    | Pong of int
  [@@deriving eq, yojson, show]
  
end
