let _ =
  let module NetworkNode = Huxiang.Node.Make((val Processes.compiled_client)) in
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  NetworkNode.start
    ~listening:[Huxiang.Node.ReliableIn Directory.clnt]
    ~network_map:Directory.network_map
    ~skey:Directory.ClientCred.secret_key
    ~pkey:Directory.ClientCred.public_key
