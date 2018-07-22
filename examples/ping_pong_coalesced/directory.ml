open Huxiang

let pong_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "pong_owner";
    pname = Name.atom "pong"
  }

let ping_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "ping_owner";
    pname = Name.atom "ping"
  }
