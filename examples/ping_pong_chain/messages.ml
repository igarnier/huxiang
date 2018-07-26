open Huxiang

module ClientToServer =
struct

  type client_id = string

  type t =
    | Ping of { uid : int64 }
  [@@deriving show, eq, yojson]


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

module ServerToClient =
struct

  type t =
    | Pong of { block_id : int64 }
  [@@deriving show, eq, yojson]

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
