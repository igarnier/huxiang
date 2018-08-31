open Huxiang


module ClientCred = (val (Crypto.(key_pair_to_cred (seeded_key_pair "client"))))

let client_node =
  {
    Address.owner = ClientCred.public_key;
    pname = Name.atom "client"
  }

module ServiceCred = (val (Crypto.(key_pair_to_cred (seeded_key_pair "service"))))

let service_node = 
  {
    Address.owner = ServiceCred.public_key;
    pname = Name.atom "service"
  }

module BrokerCred = (val (Crypto.(key_pair_to_cred (seeded_key_pair "broker"))))

let broker_node =
  {
    Address.owner = BrokerCred.public_key;
    pname = Name.atom "broker"
  }

let clnt = "tcp://127.0.0.1:5555"
let serv = "tcp://127.0.0.1:5556"
let brok = "tcp://127.0.0.1:5557"

(* We dispatch messages according to owner identities. Since in this example
   each owner has only one node, there is no ambiguity, but in general this 
   is wrong. *)
let network_map = function
  | { Address.owner; _ } when 
      Crypto.Public.equal owner client_node.owner  -> clnt
  | { Address.owner; _ } when 
      Crypto.Public.equal owner service_node.owner -> serv
  | { Address.owner; _ } when 
      Crypto.Public.equal owner broker_node.owner  -> brok
  | addr ->
    failwith @@ 
    Printf.sprintf "unknown address %s" (Address.show addr)

