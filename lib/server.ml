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
  
  let read_message socket =
    Lwt.catch (fun () ->
        Utils.read_bytes_until_cr socket >>= fun bytes ->
        let str   = Bytes.to_string bytes in
        let json  = Json.from_string str in
        Lwt.return (P.I.from_json json)
      )
      (fun exn ->
         Lwt_io.eprint "huxiang: error caught in read_message\n" >>= fun () ->
         raise exn
      )
      
  let write_message socket msg =
    Lwt.catch (fun () ->
        let json  = P.O.to_json msg in
        let bytes = Json.to_string json in
        let delim = bytes^"\r" in    
        Utils.write_bytes socket delim >>= fun _ ->
        Lwt.return ()
      )
      (fun exn ->
         Lwt_io.eprint "huxiang: error caught in write_message\n" >>= fun () ->
         raise exn
      )
  
  let process_client client_socket log_callback sstate =
    log_callback "new client connection";
    let rec loop () =
      read_message client_socket >>= fun input ->
      log_callback ("read message: "^(Json.to_string (P.I.to_json input)));
      let%lwt st, m_opt =
        try%lwt
          P.transition !sstate input
        with
        | exn ->
          Lwt_io.eprint "huxiang: error caught in P.transition\n";%lwt
          raise exn
      in
      (sstate := st;
       match m_opt with
       | None     -> loop ()
       | Some msg ->
         (log_callback ("wrote message: "^(Json.to_string (P.O.to_json msg)));
          write_message client_socket msg >>= loop))
    in
    loop ()
      
  let start ~port ~log_callback =
    let server =
      let%lwt socket = setup_server_tcp port in
      let state = ref P.initial_state in
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
        (* Lwt.async (fun () -> *)
        try%lwt
          process_client client_socket log_callback state;%lwt
          loop ()
        with
        | exn ->
          (Lwt_unix.close client_socket;%lwt
           Lwt.fail exn);
          (* ); *)
        (* loop () *)
      in
      loop ()
    in
    Lwt_main.run server
    
end
