open Batteries
open Huxiang

module I = Messages.PongMsg
module O = Messages.PingMsg

module Ping 
    : Process.S with type input  = I.t
                 and type output = O.t Address.multi_dest
=
struct

  type input  = I.t
  type output = O.t Address.multi_dest

  type state = { counter : int }
  [@@deriving show]

  let name = Name.atom "ping"

  let rec main_loop { counter } =
    Process.with_input (fun (I.Pong i) ->
        Lwt_log.info @@ "received "^(I.show (I.Pong i));%lwt
        if i = counter then
          let state  = { counter = counter + 1 } in
          let output = Address.(O.Ping (counter+1) @. Directory.pong_node) in
          Process.continue_with ~output state main_loop
        else
          Process.stop { counter }
      )

  let process state =
    let output = Address.(O.Ping 0 @. Directory.pong_node) in
    Process.without_input
      (Process.continue_with ~output state main_loop)


  let thread =
    {
      Process.state = { counter = 0 };
      move = process
    }

end

let compiled = NetProcess.compile (fun _ -> I.bin_reader_t) O.bin_writer_t (module Ping)

module PingNode = Huxiang.Node.Make((val compiled))

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  PingNode.start
    ~listening:[ReliableIn "tcp://127.0.0.1:5556"]
    ~network_map:(fun _ -> ReliableOut "tcp://127.0.0.1:5557")
    ~credentials:Directory.ping_creds
