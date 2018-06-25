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
  Lwt.catch
    (fun () ->
       Lwt_unix.bind socket addr >>= fun () ->
       Lwt_unix.listen socket 10;
       Lwt.return socket
    )
    (function e ->
       Lwt_unix.close socket >>= fun () ->
       Lwt.fail e
    )

module Make (P : Protocol_Sig) =
struct

  let read_message socket =
    Utils.read_bytes_until_cr socket >>= fun bytes ->
    let str   = Bytes.to_string bytes in
    let json  = Json.from_string str in
    Lwt.return (P.I.from_json json)

  let write_message socket msg =
    let json  = P.O.to_json msg in
    let bytes = Json.to_string json in
    let delim = bytes^"\r" in
    Utils.write_bytes socket delim >>= fun _ ->
    Lwt.return ()
  
  let process_client client_socket log_callback sstate =
    log_callback "new client connection";
    let rec loop () =
      read_message client_socket >>= fun input ->
      log_callback ("read message: "^(Json.to_string (P.I.to_json input)));
      P.transition !sstate input >>= fun (st, m_opt) ->
      sstate := st;
      match m_opt with
      | None     -> loop ()
      | Some msg ->
        (log_callback ("wrote message: "^(Json.to_string (P.O.to_json msg)));
         write_message client_socket msg >>= loop)
    in
    loop ()
      
  let start ~port ~log_callback =
    let server =
      setup_server_tcp port >>= fun socket ->
      let state = ref P.initial_state in
      let rec loop () =
        Lwt_unix.accept socket >>= fun (client_socket, client_addr) ->
        Lwt.async (fun () ->
            process_client client_socket log_callback state
          );
        loop ()
      in
      loop ()
    in
    Lwt_main.run server
    
end
