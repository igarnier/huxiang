open Huxiang

let pong_skey, pong_pkey = Crypto.seeded_key_pair "pong"

module PongCredentials = (val (Crypto.key_pair_to_cred (pong_skey, pong_pkey)))

let pong_node =
  {
    Address.owner = pong_pkey;
    pname = Name.atom "pong"
  }

let ping_skey, ping_pkey = Crypto.seeded_key_pair "ping"

module PingCredentials = (val (Crypto.key_pair_to_cred (ping_skey, ping_pkey)))

let ping_node =
  {
    Address.owner = ping_pkey;
    pname = Name.atom "ping"
  }

let oracle   = "tcp://127.0.0.1:5558"
let pingnode = "tcp://127.0.0.1:5556"
let pongnode = "tcp://127.0.0.1:5557"

let network_map = function
  | { Address.owner; _ } when Crypto.Public.equal owner ping_node.owner -> pingnode
  | { Address.owner; _ } when Crypto.Public.equal owner pong_node.owner -> pongnode
  | _ ->
    failwith "invalid address"

