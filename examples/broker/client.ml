open Huxiang
open Directory

let network_map = function
  | { Address.owner; _ } when 
      Crypto.Public.equal owner broker_node.owner -> 
    Huxiang.Node.Publish brok_clnt
  | addr ->
    failwith @@ 
    Printf.sprintf "unknown address %s" (Address.show addr)

let _ =
  let module NetworkNode = Huxiang.Node.Make((val Processes.compiled_client)) in
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  NetworkNode.start
    ~listening:[]
    ~network_map
    ~skey:Directory.ClientCred.secret_key
    ~pkey:Directory.ClientCred.public_key
