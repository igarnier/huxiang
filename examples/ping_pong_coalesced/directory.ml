open Huxiang

let ping_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "ping_owner";
    pname = Name.atom "ping"
  }

let pong_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "pong_owner";
    pname = Name.atom "pong"
  }

let oracle   = "tcp://127.0.0.1:5558"
let pingnode = "tcp://127.0.0.1:5556"
let pongnode = "tcp://127.0.0.1:5557"

let network_map = function
  | { Process.Address.owner } when owner = ping_node.owner -> pingnode
  | { Process.Address.owner } when owner = pong_node.owner -> pongnode
  | _ ->
    failwith "invalid address"

