let _ = Random.self_init ()

module NetworkNode = Huxiang.Node.Make(Processes.PingPongForPong)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  NetworkNode.start
    ~listening:Directory.pongnode
    ~network_map:Directory.network_map
    ~skey:Directory.pong_skey
    ~pkey:Directory.pong_pkey
