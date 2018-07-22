open Huxiang

module Nothing =
struct

  type t = unit
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
      failwith "pingmsg/deserialize: wrong path"

end

module ClientToBroker =
struct
  
  type t =
    | Payement of int
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
      failwith "pingmsg/deserialize: wrong path"

end

module BrokerToService =
struct
  
  type t =
    | BulkPayement of int
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
      failwith "pingmsg/deserialize: wrong path"

end
