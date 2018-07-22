open Huxiang

let _ = Random.self_init ()

module NetworkNode = Huxiang.Node.Make(Processes.PingPongForPing)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let pingnode = "tcp://127.0.0.1:5556" in
  let pongnode = "tcp://127.0.0.1:5557" in
  let network_map = function
    | { Process.Address.owner } when owner = Directory.ping_node.owner -> pingnode
    | { Process.Address.owner } when owner = Directory.pong_node.owner -> pongnode
    | _ ->
      failwith "invalid address"
  in
  NetworkNode.start_dynamic
    ~listening:pingnode
    ~network_map



