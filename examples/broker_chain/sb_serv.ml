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
  NetworkNode.start_dynamic
    ~listening:Directory.serv
    ~network_map:Directory.network_map



