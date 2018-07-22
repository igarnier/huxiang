open Huxiang


module NetworkNode = Huxiang.Node.Make(Processes.SB_Serv)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        (* ~template:"$(date).$(milliseconds) [$(level)] $(message)" *)
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let clnt = "tcp://127.0.0.1:5555" in
  let serv = "tcp://127.0.0.1:5556" in
  let brok = "tcp://127.0.0.1:5557" in
  let network_map = function
    | { Process.Address.owner } when Types.equal_public_identity owner Directory.client_node.owner -> clnt
    | { Process.Address.owner } when Types.equal_public_identity owner Directory.service_node.owner -> serv
    | { Process.Address.owner } when Types.equal_public_identity owner Directory.broker_node.owner -> brok
    | addr ->
      failwith @@ 
      Printf.sprintf "unknown address %s" (Process.Address.show addr)
  in
  NetworkNode.start_dynamic
    ~listening:serv
    ~network_map



