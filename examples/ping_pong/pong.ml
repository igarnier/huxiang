open Batteries
open Huxiang.Types

module PongProcess : Process =
struct

  module I = Messages.PingMsg
  module O = Messages.PongMsg
               
  type state = unit

  let initial_state   = ()

  let rec process =
    Input (fun state (I.Ping i) ->
        Lwt.return (state, Some (O.Pong i), process)
      )
      
end

module PongNode = Huxiang.Node.Make(PongProcess)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());  
  PongNode.start_mcast
    ~listening:"tcp://127.0.0.1:5557"
    ~outgoing:["tcp://127.0.0.1:5556"]

