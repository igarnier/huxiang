open Huxiang

module PingPongForPing = 
  Coalesce.Prod
    (Ping.PingProcess)
    (Pong.PongProcess)
    (TrivialLeader)
    (struct let selector = Coalesce.Left end)

module NetworkNode = Huxiang.Node.Make(PingPongForPing)

let _ =
  let _  = Printf.eprintf "Starting pp_ping\n%!" in
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let pingnode = "tcp://127.0.0.1:5556" in
  let pongnode = "tcp://127.0.0.1:5557" in
  let out_dispatch = function
    | PingPongForPing.O.Notification _ -> [pongnode]
    | _ (* regular outputs *) -> [pingnode; pongnode]
  in
  NetworkNode.start_dynamic
    ~listening:pingnode
    ~out_dispatch



