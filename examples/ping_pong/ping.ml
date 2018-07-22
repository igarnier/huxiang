open Batteries
open Huxiang
open Huxiang.Types

module PingProcess : Process.S =
struct

  module I = Messages.PongMsg
  module O = Messages.PingMsg
               
  type state = { counter : int }
  [@@deriving show]

  let name = Process.Name.atom "ping"

  let rec main_loop  { counter } =
    Process.with_input (fun (I.Pong i) ->
        if i = counter then
          let state  = { counter = counter + 1 } in
          let output =
            { Process.Address.msg = O.Ping (counter + 1); 
              dests = [ (Directory.pong_node, Root) ] }
          in
          Process.continue_with ~output state main_loop
        else
          Process.stop { counter }
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
      Process.state = { counter = 0 }
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
