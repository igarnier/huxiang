module PingMsg =
struct
  
  type t =
    | Ping of int
  [@@deriving eq, yojson]

  let of_yojson_exn j =
    match of_yojson j with
    | Ok v      -> v
    | Error msg ->
      failwith msg

end

module PongMsg =
struct

  type t =
    | Pong of int
  [@@deriving eq, yojson]

  let of_yojson_exn j =
    match of_yojson j with
    | Ok v      -> v
    | Error msg ->
      failwith msg
  
end
