open Huxiang.Types
open Huxiang

module Oracle =
struct

  module I =
    struct
      
      type t = unit
      [@@deriving yojson,eq,show]

    end

  module O =
  struct
    
    type t = Leader of TrivialLeader.t
    [@@deriving yojson,eq,show]

  end

  type state = unit

  let show_state i = "()"
    
  let rec main_loop state =
    Process.without_input
      (Lwt_unix.sleep 3.0;%lwt
       Process.continue_with ~output:(O.Leader ()) state main_loop)

  let thread =
    {
      Process.move = main_loop;
      state = ()
    }

end

module OracleNode = Huxiang.Node.Make(Oracle)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let oracle = "tcp://127.0.0.1:5558" in
  let serv   = "tcp://127.0.0.1:5556" in
  let brok   = "tcp://127.0.0.1:5557" in
  let get_next () = Random.bool () in
  let out_dispatch = function
    |  _ -> 
      if get_next () then
        (Printf.eprintf "Server is elected leader\n%!";
         [serv])
      else
        (Printf.eprintf "Broker is elected leader\n%!";
         [brok])
  in
  OracleNode.start_dynamic
    ~listening:oracle
    ~out_dispatch
