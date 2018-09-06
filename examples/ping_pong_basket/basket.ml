open Batteries
open Huxiang

module I =
struct
  type t =
    | FromPing of Messages.PingMsg.t
    | FromPong of Messages.PongMsg.t
  [@@deriving show, eq, bin_io]
end

module O = Messages.Nothing

module Basket 
    : Process.S with type input  = I.t
                 and type output = O.t Address.multi_dest
=
struct

  type input  = I.t
  type output = O.t Address.multi_dest

  type state = unit
  [@@deriving show]

  let name = Name.atom "ping"

  let rec process state =
    Process.with_input (function
        | I.FromPing (Ping i) ->
          Lwt_io.printf "received Ping(%d)\n" i;%lwt
          Process.continue_with state process          
        | I.FromPong (Pong i) ->
          Lwt_io.printf "received Pong(%d)\n" i;%lwt
          Process.continue_with state process
      )

  let thread =
    {
      Process.state = ();
      move = process
    }

end

let input pkeyopt =
  match pkeyopt with
  | None ->
    failwith "no public key on input"
  | Some pkey ->
    if Crypto.Public.equal pkey Directory.ping_pkey then
      let inject x = I.FromPing x in
      Utils.map_reader Messages.PingMsg.bin_reader_t inject
    else if Crypto.Public.equal pkey Directory.pong_pkey then
      let inject x = I.FromPong x in
      Utils.map_reader Messages.PongMsg.bin_reader_t inject
    else
      failwith "unknown public key"

let compiled = NetProcess.compile input O.bin_writer_t (module Basket)

module BasketNode = Huxiang.Node.Make((val compiled))

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  BasketNode.start
    ~listening:"tcp://127.0.0.1:5558"
    ~network_map:(fun _ -> "")
    ~skey:Directory.ping_skey
    ~pkey:Directory.ping_pkey
