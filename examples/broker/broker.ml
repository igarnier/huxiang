open Batteries

module Node = Huxiang.Node.Make((val Processes.compiled_broker))

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let serv = "tcp://127.0.0.1:5558" in
  Node.start
    ~listening:[Huxiang.Node.ReliableIn "tcp://127.0.0.1:5557"]
    ~network_map:(fun _ -> serv)
    ~skey:Directory.BrokerCred.secret_key
    ~pkey:Directory.BrokerCred.public_key
