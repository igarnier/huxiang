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
