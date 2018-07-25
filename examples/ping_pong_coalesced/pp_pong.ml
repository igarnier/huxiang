open Huxiang

let _ = Random.self_init ()

module NetworkNode = Huxiang.Node.Make(Processes.PingPongForPong)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  NetworkNode.start_dynamic
    ~listening:Directory.pongnode
    ~network_map:Directory.network_map
