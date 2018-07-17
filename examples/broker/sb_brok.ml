open Huxiang

module SB_Brok = 
  Coalesce.Prod
    (Processes.Service)
    (Processes.Broker)
    (TrivialLeader)
    (struct let selector = Coalesce.Right end)

module NetworkNode = Huxiang.Node.Make(SB_Brok)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        (* ~template:"$(date).$(milliseconds) [$(level)] $(message)" *)
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let serv = "tcp://127.0.0.1:5556" in
  let brok = "tcp://127.0.0.1:5557" in
  let out_dispatch = function
    | Coalesce.Notification _ -> [serv]
    | _ (* regular outputs *) -> [serv; brok]
  in
  NetworkNode.start_dynamic
    ~listening:brok
    ~out_dispatch



