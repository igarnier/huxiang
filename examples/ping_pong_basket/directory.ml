open Huxiang

let pong_skey, pong_pkey = Crypto.seeded_key_pair "pong"

module PongCredentials = (val (Crypto.key_pair_to_cred (pong_skey, pong_pkey)))

let _ =
  Printf.eprintf "pong pkey: %s\n%!" (Crypto.Public.show pong_pkey)

let pong_node =
  {
    Address.owner = pong_pkey;
    pname = Name.atom "pong"
  }

let ping_skey, ping_pkey = Crypto.seeded_key_pair "ping"

module PingCredentials = (val (Crypto.key_pair_to_cred (ping_skey, ping_pkey)))

let _ =
  Printf.eprintf "ping pkey: %s\n%!" (Crypto.Public.show ping_pkey)

let ping_node =
  {
    Address.owner = ping_pkey;
    pname = Name.atom "ping"
  }

let basket_skey, basket_pkey = Crypto.seeded_key_pair "basket"

module BasketCredentials = (val (Crypto.key_pair_to_cred (basket_skey, basket_pkey)))

let _ =
  Printf.eprintf "basket pkey: %s\n%!" (Crypto.Public.show basket_pkey)

let basket_node =
  {
    Address.owner = basket_pkey;
    pname = Name.atom "basket"
  }
