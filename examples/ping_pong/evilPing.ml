open Batteries
open Huxiang
open Huxiang.Types

module PingProcess =
struct

  module I = Messages.PongMsg
  module O = Messages.PingMsg
               
  type state = unit
  [@@deriving show]

  let name = Process.Name.atom "ping"

  let rec main_loop () =
    Process.with_input (fun (I.Pong i) ->
        let output =
          { Process.Address.msg = O.Ping (Random.int 42); 
            dests = [ (Directory.pong_node, Root) ] }
        in
        Process.continue_with ~output () main_loop
      )

  let process state =
    let output =
      { Process.Address.msg = O.Ping 0; 
        dests = [ (Directory.pong_node, Root) ] }
    in
    Process.without_input
      (Process.continue_with ~output state main_loop)

  let thread =
    {
      Process.move  = process;
      Process.state = ()
    }
              
end

module PingNode = Huxiang.Node.Make(PingProcess)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  PingNode.start_dynamic
    ~listening:"tcp://127.0.0.1:5556"
    ~network_map:(fun _ -> "tcp://127.0.0.1:5557")
