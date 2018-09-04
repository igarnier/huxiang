open Huxiang


module ClientCred = (val Crypto.(key_pair_to_cred (seeded_key_pair "client")))

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

module MomCred = (val Crypto.(key_pair_to_cred (seeded_key_pair "mother")))

let mother_node =
  {
    Address.owner = MomCred.public_key;
    pname = Name.atom "mother"
  }


let clnt = "tcp://127.0.0.1:5555"
let serv = "tcp://127.0.0.1:5556"
let brok = "tcp://127.0.0.1:5557"

(* we use the same node to serve both processes but in reality these should be
   distinct ones for security reasons. *)
let mother = "tcp://127.0.0.1:5559"

let network_map = 
  fun ({ Address.owner; pname } as addr) ->
  if Crypto.Public.equal owner client_node.owner then
    clnt
  else if Crypto.Public.equal owner service_node.owner then
    serv
  else if Crypto.Public.equal owner broker_node.owner then
    brok
  else if Name.equal pname (Name.atom "mother") then
    mother
  else
    failwith @@ 
    Printf.sprintf "unknown address %s" (Address.show addr)
