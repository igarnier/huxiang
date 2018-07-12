open Batteries
open Huxiang.Types

module PingProcess =
struct

  module I = Messages.PongMsg
  module O = Messages.PingMsg
               
  type state =
    | Alive of { counter : int }
    | Dead

  let initial_state   = Alive { counter = 0 }
  let initial_message = Some (O.Ping 0)

  let transition state (I.Pong i) =
    match state with
    | Dead              -> Lwt.return (Dead, None)
    | Alive { counter } ->
      if i = counter then
        let state = Alive { counter = counter + 1 } in
        Lwt.return (state, Some (O.Ping (counter + 1)))
      else
        Lwt.return (Dead, None)
      
end

module PingNode = Huxiang.Node.Make(PingProcess)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  PingNode.start_mcast
    ~listening:"tcp://127.0.0.1:5556"
    ~outgoing:["tcp://127.0.0.1:5557"]
