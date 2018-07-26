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
  NetworkNode.start_dynamic
    ~listening:Directory.clnt
    ~network_map:Directory.network_map
