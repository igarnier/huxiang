open Huxiang

let pong_skey, pong_pkey = Crypto.seeded_key_pair "pong"

let pong_node =
  let open Process in
  {
    Address.owner = pong_pkey;
    pname = Name.atom "pong"
  }

let ping_skey, ping_pkey = Crypto.seeded_key_pair "ping"

let ping_node =
  let open Process in
  {
    Address.owner = ping_pkey;
    pname = Name.atom "ping"
  }

let oracle   = "tcp://127.0.0.1:5558"
let pingnode = "tcp://127.0.0.1:5556"
let pongnode = "tcp://127.0.0.1:5557"

let network_map = function
  | { Address.owner } when Crypto.Public.equal owner ping_node.owner -> pingnode
  | { Address.owner } when Crypto.Public.equal owner pong_node.owner -> pongnode
  | _ ->
    failwith "invalid address"

