open Batteries
(* open Sodium *)
open Lwt.Infix

open Types

module Json = Yojson.Safe

let setup_server_tcp port =
  let domain = Lwt_unix.PF_INET in
  let socktp = Lwt_unix.SOCK_STREAM in
  let socket = Lwt_unix.socket domain socktp 0 in
  let addr   = Unix.ADDR_INET(Unix.inet_addr_loopback, port) in
  Lwt_unix.setsockopt socket Unix.TCP_NODELAY true;
  Lwt_unix.setsockopt socket Unix.SO_KEEPALIVE true;
  try%lwt
    Lwt_unix.bind socket addr;%lwt
    Lwt_unix.listen socket 10;
    Lwt.return socket
  with
  | exn ->
    (Lwt_unix.close socket;%lwt
     Lwt.fail exn)


module Make (P : Protocol_Sig) =
struct
  
  
  let process_client client_addr client_socket log_callback keepalive_dt sstate =
    log_callback "new client connection";
    let state =
      let open Keepalive in
      {
        socket       = client_socket;
        state        = sstate;
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
    Keepalive.loop state
      
  let start ~port ~log_callback ~keepalive_dt =
    let server =
      let%lwt socket = setup_server_tcp port in
      let state = Lwt_mvar.create P.initial_state in
      let rec loop () =
        Lwt_io.print "server: waiting for client\n";%lwt
        let%lwt client_socket, client_addr =
          try%lwt
            Lwt_unix.accept socket
          with
          | exn ->
            (Lwt_io.print "exception caught in accept";%lwt
             Lwt.fail exn)
        in
        Lwt.async (fun () ->
            process_client client_addr client_socket log_callback keepalive_dt state
          );
        loop ()
      in
      loop ()
    in
    Lwt_main.run server
    
end
