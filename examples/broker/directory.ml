open Huxiang

let client_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "client_owner";
    pname = Name.atom "client"
  }

let service_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "service_owner";
    pname = Name.atom "service"
  }

let broker_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "broker_owner";
    pname = Name.atom "broker"
  }

let service_node_product =
  let open Process in
  {
    Address.owner = Bytes.of_string "service_owner";
    pname = Name.prod [service_node.pname; broker_node.pname]
  }

let broker_node_product =
  let open Process in
  {
    Address.owner = Bytes.of_string "broker_owner";
    pname = Name.prod [service_node.pname; broker_node.pname]
  }

let clnt = "tcp://127.0.0.1:5555"
let serv = "tcp://127.0.0.1:5556"
let brok = "tcp://127.0.0.1:5557"

(* We dispatch messages according to owner identities. Since in this example
   each owner has only one node, there is no ambiguity, but in general this 
   is wrong. *)
let network_map = function
  | { Process.Address.owner } when 
      Types.equal_public_identity owner client_node.owner  -> clnt
  | { Process.Address.owner } when 
      Types.equal_public_identity owner service_node.owner -> serv
  | { Process.Address.owner } when 
      Types.equal_public_identity owner broker_node.owner  -> brok
  | addr ->
    failwith @@ 
    Printf.sprintf "unknown address %s" (Process.Address.show addr)

