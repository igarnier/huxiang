module NetworkNode = Huxiang.Node.Make(Processes.SB_Serv)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  NetworkNode.start
    ~listening:[Huxiang.Node.ReliableIn Directory.serv]
    ~network_map:Directory.network_map
    ~skey:Directory.ServiceCred.secret_key
    ~pkey:Directory.ServiceCred.public_key
