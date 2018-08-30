let _ = Random.self_init ()

module NetworkNode = Huxiang.Node.Make(Processes.PingPongForPing)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  NetworkNode.start
    ~listening:Directory.pingnode
    ~network_map:Directory.network_map
    ~skey:Directory.ping_skey
    ~pkey:Directory.ping_pkey
