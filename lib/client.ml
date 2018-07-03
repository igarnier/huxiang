open Batteries
open Lwt.Infix

open Types

module Json = Yojson.Safe

let setup_client_tcp port =
  Lwt.catch (fun () ->
      let server_addr = Lwt_unix.ADDR_INET(Unix.inet_addr_loopback, port) in
      let domain = Lwt_unix.PF_INET in
      let socktp = Lwt_unix.SOCK_STREAM in
      let socket = Lwt_unix.socket domain socktp 0 in
      Lwt_unix.setsockopt socket Unix.TCP_NODELAY true;
      Lwt_unix.setsockopt socket Unix.SO_KEEPALIVE true;      
      Lwt_unix.connect socket server_addr >>= fun _ ->
      Lwt.return socket
    )
    (fun exn ->
       Lwt_io.eprint "huxiang: error caught in setup_client_tcp\n" >>= fun () ->
       raise exn
    )

let close socket =
  Lwt_unix.shutdown socket Unix.SHUTDOWN_ALL

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
      
  let start ~port ~(log_callback : string -> unit) ~initial_message =
    let client =
      let%lwt socket = setup_client_tcp port in
      log_callback "setup finished";
      let rec loop cstate =
        let%lwt message = read_message socket in
        log_callback ("read message: "^(Json.to_string (P.I.to_json message)));
        let%lwt cstate, m_opt =
          try%lwt
            P.transition cstate message
          with
          | exn -> 
            (Lwt_io.eprint "huxiang: error caught in P.transition\n";%lwt
             (try Lwt.return (close socket)
              with
              | _->
                Lwt_io.eprint "huxiang: further error caught when closing socket\n");%lwt
             Lwt.fail exn)
        in
        match m_opt with
        | None     -> loop cstate
        | Some msg ->
          (log_callback ("wrote message: "^(Json.to_string (P.O.to_json msg)));
           write_message socket msg;%lwt
           loop cstate)
      in
      write_message socket initial_message;%lwt
      log_callback "initial message written";
      loop P.initial_state
    in
    Lwt_main.run client
  
end
