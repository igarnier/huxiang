open Huxiang

module PingMsg =
struct
  
  type t =
    | Ping of int
  [@@deriving eq, show, yojson]

  let serialize x =
    Yojson.Safe.to_string (to_yojson x)

  let deserialize s pth =    
    match pth with
    | Process.Address.Root ->
      (match of_yojson (Yojson.Safe.from_string s) with
       | Ok x -> x
       | Error s -> failwith s)
    | _ ->
      failwith "pingmsg/deserialize: wrong path"

end

module PongMsg =
struct

  type t =
    | Pong of int
  [@@deriving eq, yojson, show]

  let serialize x =
    Yojson.Safe.to_string (to_yojson x)

  let deserialize s pth =    
    match pth with
    | Process.Address.Root ->
      (match of_yojson (Yojson.Safe.from_string s) with
       | Ok x -> x
       | Error s -> failwith s)
    | _ ->
      failwith "pongmsg/deserialize: wrong path"
  
end
