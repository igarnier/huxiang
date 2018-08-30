open Huxiang

let pong_skey, pong_pkey = Crypto.seeded_key_pair "pong"

let pong_node =
  {
    Address.owner = pong_pkey;
    pname = Name.atom "pong"
  }

let ping_skey, ping_pkey = Crypto.seeded_key_pair "ping"

let ping_node =
  {
    Address.owner = ping_pkey;
    pname = Name.atom "ping"
  }
