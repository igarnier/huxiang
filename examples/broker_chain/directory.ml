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


let service_mother_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "service_owner";
    pname = Name.atom "mother"
  }

let broker_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "broker_owner";
    pname = Name.atom "broker"
  }


let broker_mother_node =
  let open Process in
  {
    Address.owner = Bytes.of_string "broker_owner";
    pname = Name.atom "mother"
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
let oracle = "tcp://127.0.0.1:5558"

(* we use the same node to serve both processes but in reality these should be
   distinct ones for security reasons. *)
let service_mother = "tcp://127.0.0.1:5559"
let broker_mother = "tcp://127.0.0.1:5559"

let network_map = 
  fun ({ Process.Address.owner; pname } as addr) ->
  if Types.equal_public_identity owner client_node.owner then
    clnt
  else if Types.equal_public_identity owner service_node.owner then
    if Process.Name.equal pname (Process.Name.atom "mother") then
      service_mother
    else
      serv
  else if Types.equal_public_identity owner broker_node.owner then
    if Process.Name.equal pname (Process.Name.atom "mother") then
      broker_mother
    else
      brok
  else
    failwith @@ 
    Printf.sprintf "unknown address %s" (Process.Address.show addr)
