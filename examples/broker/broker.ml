open Batteries
open Huxiang.Types

module Node = Huxiang.Node.Make(Processes.Broker)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  Node.start_dynamic
    ~listening:"tcp://127.0.0.1:5557"
    ~out_dispatch:(fun _ -> ["tcp://127.0.0.1:5558"])
