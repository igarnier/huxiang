open Batteries
open Huxiang.Types

module Node = Huxiang.Node.Make(Processes.Client)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let serv = "tcp://127.0.0.1:5556" in
  let brok = "tcp://127.0.0.1:5557" in
  Node.start_dynamic
    ~listening:"tcp://127.0.0.1:5555"
    ~out_dispatch:(fun _ -> [serv; brok])
