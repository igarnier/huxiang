module PingMsg =
struct
  
  type t =
    | Ping of int
  [@@deriving yojson]

  let to_json = to_yojson

  let from_json j =
    match of_yojson j with
    | Ok v      -> v
    | Error msg ->
      failwith msg

end

module PongMsg =
struct

  type t =
    | Pong of int
  [@@deriving yojson]

  let to_json = to_yojson

  let from_json j =
    match of_yojson j with
    | Ok v      -> v
    | Error msg ->
      failwith msg
  
end
