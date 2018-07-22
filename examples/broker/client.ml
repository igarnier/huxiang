open Batteries
open Huxiang
open Huxiang.Types


let _ =
  if Array.length Sys.argv != 2 then
    failwith "usage: ./client before or ./client after";
  let client =
    match Sys.argv.(1) with
    | "before" ->
      (module Processes.Client : Process.S)
    | "after"  ->
      (module Processes.ClientAfterProduct : Process.S)
    | _ ->
      failwith "usage: ./client before or ./client after"
  in
  let module NetworkNode = Huxiang.Node.Make((val client : Process.S)) in
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let clnt = "tcp://127.0.0.1:5555" in
  let serv = "tcp://127.0.0.1:5556" in
  let brok = "tcp://127.0.0.1:5557" in
  let network_map = function
    | { Process.Address.owner } when Types.equal_public_identity owner Directory.client_node.owner -> clnt
    | { Process.Address.owner } when Types.equal_public_identity owner Directory.service_node.owner -> serv
    | { Process.Address.owner } when Types.equal_public_identity owner Directory.broker_node.owner -> brok
    | addr ->
      failwith @@ 
      Printf.sprintf "unknown address %s" (Process.Address.show addr)
  in
  NetworkNode.start_dynamic
    ~listening:"tcp://127.0.0.1:5555"
    ~network_map
