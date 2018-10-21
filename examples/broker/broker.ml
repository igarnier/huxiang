open Batteries
open Huxiang
open Directory

let network_map = function
  | { Address.owner; _ } when 
      Crypto.Public.equal owner service_node.owner -> 
    Node.ReliableOut Directory.serv
  | addr ->
    failwith @@ 
    Printf.sprintf "unknown address %s" (Address.show addr)


module Node = Huxiang.Node.Make((val Processes.compiled_broker))

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  Node.start
    ~listening:[Huxiang.Node.Subscribe Directory.brok_clnt]
    ~network_map:network_map
    ~skey:Directory.BrokerCred.secret_key
    ~pkey:Directory.BrokerCred.public_key
