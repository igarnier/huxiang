open Batteries
open Huxiang
open Huxiang.Types

module I = Messages.PongMsg
module O = Messages.PingMsg

module Ping 
    : Process.S with type input  = I.t
                 and type output = O.t Address.multi_dest
=
struct

  type input  = I.t
  type output = O.t Address.multi_dest

  type state = unit
  [@@deriving show]

  let name = Name.atom "ping"

  let rec main_loop () =
    Process.with_input (fun (I.Pong i) ->
        let output = Address.(O.Ping (Random.int 42) @. Directory.pong_node) in
        Process.continue_with ~output () main_loop
      )

  let process state =
    let output = Address.(O.Ping 0 @. Directory.pong_node) in
    Process.without_input
      (Process.continue_with ~output state main_loop)

  let thread =
    {
      Process.move  = process;
      Process.state = ()
    }

end

module NetPing =
  NetProcess.Compile
    (struct
      include I
      let deserializer _ = bin_reader_t
    end)
    (struct
      include O
      let serializer = bin_writer_t
    end)
    (Ping)

module PingNode = Huxiang.Node.Make(NetPing)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  PingNode.start
    ~listening:"tcp://127.0.0.1:5556"
    ~network_map:(fun _ -> "tcp://127.0.0.1:5557")
    ~skey:Directory.ping_skey
    ~pkey:Directory.ping_pkey
