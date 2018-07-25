open Batteries
open Huxiang
open Huxiang.Types

module PongProcess =
struct

  module I = Messages.PingMsg
  module O = Messages.PongMsg
               
  type state = unit
  [@@deriving show]

  let name = Process.Name.atom "pong"

  let rec main_loop state =
    Process.with_input (fun (I.Ping i) ->
        let output = Process.(O.Pong i @ Directory.ping_node) in
        Process.continue_with ~output state main_loop
      )

  let thread =
    {
      Process.move  = main_loop;
      Process.state = ()
    }
      
end

module PongNode = Huxiang.Node.Make(PongProcess)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());  
  PongNode.start_dynamic
    ~listening:"tcp://127.0.0.1:5557"
    ~network_map:(fun _ -> "tcp://127.0.0.1:5556")

