module NetworkNode = Huxiang.Node.Make(Processes.SB_Serv)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  NetworkNode.start
    ~listening:Directory.serv
    ~network_map:Directory.network_map
    ~skey:Directory.ServiceCred.secret_key
    ~pkey:Directory.ServiceCred.public_key
