open Huxiang

module PingPongForPong = 
  Coalesce.Prod
    (Processes.Ping)
    (Processes.Pong)
    (TrivialLeader)
    (struct let selector = Coalesce.Right end)

module NetworkNode = Huxiang.Node.Make(PingPongForPong)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let pingnode = "tcp://127.0.0.1:5556" in
  let pongnode = "tcp://127.0.0.1:5557" in
  let out_dispatch = function
    | Coalesce.Notification _ -> [pingnode]
    | _ (* regular outputs *) -> [pingnode; pongnode]
  in
  NetworkNode.start_dynamic
    ~listening:pongnode
    ~out_dispatch
