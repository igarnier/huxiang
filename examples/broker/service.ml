open Batteries

(* We dispatch messages according to owner identities. Since in this example
   each owner has only one node, there is no ambiguity, but in general this 
   is wrong. *)

module Node = Huxiang.Node.Make((val Processes.compiled_service))

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  Node.start
    ~listening:[Huxiang.Node.ReliableIn Directory.serv]
    ~network_map:(fun _ -> failwith "network map should not be called")
    ~skey:Directory.ServiceCred.secret_key
    ~pkey:Directory.ServiceCred.public_key
