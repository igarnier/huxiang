open Batteries
open Lwt.Infix

open Types

module Json = Yojson.Safe

let setup_client_tcp port =
  try%lwt
    let server_addr = Lwt_unix.ADDR_INET(Unix.inet_addr_loopback, port) in
    let domain = Lwt_unix.PF_INET in
    let socktp = Lwt_unix.SOCK_STREAM in
    let socket = Lwt_unix.socket domain socktp 0 in
    Lwt_unix.setsockopt socket Unix.TCP_NODELAY true;
    Lwt_unix.setsockopt socket Unix.SO_KEEPALIVE true;      
    Lwt_unix.connect socket server_addr;%lwt
    Lwt.return socket
  with
  | exn ->
    Lwt_io.eprint "huxiang: error caught in setup_client_tcp\n" >>= fun () ->
    Lwt.fail exn


module Make (P : Protocol_Sig) =
struct

  let start ~port ~(log_callback : string -> unit) ~initial_message ~keepalive_dt =
    let client =
      let%lwt socket = setup_client_tcp port in
      log_callback "setup finished";
      let state =
        let open Keepalive in
        {
          socket;
          state        = Lwt_mvar.create P.initial_state;
          status       = LinkAvailable;
          nonce        = 0L;
          keepalive_dt;
          timeouts     = 0;
          in_from_json = P.I.from_json;
          in_to_json   = P.I.to_json;
          out_to_json  = P.O.to_json;
          transition   = P.transition;
          log_callback
        }
      in
      log_callback "initial message written";
      Transport.write_message socket (Transport.Json (P.O.to_json initial_message));%lwt
      Keepalive.loop state
    in
    Lwt_main.run client
  
end
