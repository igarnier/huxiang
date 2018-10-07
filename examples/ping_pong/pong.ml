open Batteries
open Huxiang

module O = Messages.PongMsg
module I = Messages.PingMsg


module Pong =
struct

  type input  = I.t
  type output = O.t Address.multi_dest
               
  type state = unit
  [@@deriving show]

  let name = Name.atom "pong"

  let rec main_loop state =
    Process.with_input (fun (I.Ping i) ->
        Lwt_log.info @@ "received "^(I.show (I.Ping i));%lwt
        let output = Address.(O.Pong i @. Directory.ping_node) in
        Process.continue_with ~output state main_loop
      )

  let thread =
    {
      Process.move  = main_loop;
      Process.state = ()
    }
      
end

let compiled = NetProcess.compile (fun _ -> I.bin_reader_t) O.bin_writer_t (module Pong)

module PongNode = Huxiang.Node.Make((val compiled))

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());  
  PongNode.start
    ~listening:[ReliableIn "tcp://127.0.0.1:5557"]
    ~network_map:(fun _ -> "tcp://127.0.0.1:5556")
    ~skey:Directory.pong_skey
    ~pkey:Directory.pong_pkey

