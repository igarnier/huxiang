open Batteries
open Huxiang
open Huxiang.Types

module PingProcess =
struct

  module I = Messages.PongMsg
  module O = Messages.PingMsg
               
  type state =
    | Alive of { counter : int }
    | Dead
  [@@deriving show]

  let rec main_loop state =
    Process.with_input (fun (I.Pong i) ->
        match state with
        | Alive { counter } when i = counter ->
          let state = Alive { counter = counter + 1 } in
          Process.continue_with ~output:(O.Ping (counter + 1)) state main_loop
        | _ ->
          let state = Dead in
          Process.continue_with state main_loop
      )

  let process state =
    Process.without_input
      (Process.continue_with ~output:(O.Ping 0) state main_loop)

  let thread =
    {
      Process.move  = process;
      Process.state = Alive { counter = 0 }
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
    ~out_dispatch:(fun _ -> ["tcp://127.0.0.1:5557"])
