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
          let output = Process.(O.Ping (counter+1) @ Directory.pong_node) in
          Process.continue_with ~output state main_loop
        else
          Process.stop { counter }
      )

  let process state =
    let output = Process.(O.Ping 0 @ Directory.pong_node) in
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
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  PingNode.start_dynamic
    ~listening:"tcp://127.0.0.1:5556"
    ~network_map:(fun _ -> "tcp://127.0.0.1:5557")
