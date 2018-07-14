open Huxiang.Types


module Oracle : Process =
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

  let initial_state = ()

  let show_state i = "()"
    
  let rec process =
    NoInput (fun state ->
        Lwt_unix.sleep 3.0;%lwt
        Lwt.return (state, Some (O.Leader ()), process)
      )

end

module OracleNode = Huxiang.Node.Make(Oracle)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Info in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  let oracle   = "tcp://127.0.0.1:5558" in
  let pingnode = "tcp://127.0.0.1:5556" in
  let pongnode = "tcp://127.0.0.1:5557" in
  let get_next () = Random.bool () in
  let out_dispatch = function
    |  _ -> 
      if get_next () then
        (Printf.eprintf "Ping is elected leader\n%!";
         [pingnode])
      else
        (Printf.eprintf "Pong is elected leader\n%!";
         [pongnode])
  in
  OracleNode.start_dynamic
    ~listening:oracle
    ~out_dispatch
