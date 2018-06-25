open Batteries
open Lwt.Infix

open Types

module Json = Yojson.Safe

let setup_client_tcp port =
  let server_addr = Lwt_unix.ADDR_INET(Unix.inet_addr_loopback, port) in
  let domain = Lwt_unix.PF_INET in
  let socktp = Lwt_unix.SOCK_STREAM in
  let socket = Lwt_unix.socket domain socktp 0 in
  Lwt_unix.setsockopt socket Unix.TCP_NODELAY true;
  Lwt_unix.connect socket server_addr >>= fun _ ->
  Lwt.return socket

let close socket =
  Lwt_unix.shutdown socket Unix.SHUTDOWN_ALL

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
      
  let start ~port ~(log_callback : string -> unit) ~initial_message =
    let client =
      setup_client_tcp port >>= fun socket ->
      log_callback "setup finished";
      let rec loop cstate =
        read_message socket >>= fun message ->
        log_callback ("read message: "^(Json.to_string (P.I.to_json message)));
        P.transition cstate message >>= fun (cstate, m_opt) ->
        match m_opt with
        | None     -> loop cstate
        | Some msg ->
          (log_callback ("wrote message: "^(Json.to_string (P.O.to_json msg)));
           write_message socket msg >>= fun _ -> loop cstate)
      in
      write_message socket initial_message >>= fun _ ->
      log_callback "initial message written";
      loop P.initial_state
    in
    Lwt_main.run client
  
end

(* let send_orders socket orders =
 *   let json = Order.list_to_json orders in
 *   let str  = Order.Json.to_string json in
 *   Utils.write_bytes socket str
 * 
 * let _ =
 *   let socket = connect () in
 *   Printf.printf "Connected to server\n%!";
 *   let input  = Unix.input_of_descr socket in
 *   let orders =
 *     Order.([
 *         Create { user = "Ilias" };
 *         Create { user = "Huisong" };
 *         Transfer { src = "Ilias"; dst = "Huisong"; amount = 200 };
 *         GetBalance { user = "Ilias" };
 *         GetBalance { user = "Huisong" }
 *       ])
 *   in
 *   send_orders socket orders;
 *   Unix.shutdown socket Unix.SHUTDOWN_SEND;
 *   Printf.printf "Waiting for server feedback\n%!";
 *   let result =
 *     let str = Bytes.to_string (Utils.read_bytes socket) in
 *     Order.Json.from_string str
 *   in
 *   Printf.printf "result:\n%s\n%!" (Order.Json.to_string result) *)
