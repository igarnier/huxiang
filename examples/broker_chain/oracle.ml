open Huxiang.Types
open Huxiang

let face =
  let open Process in
  [|
    ({
      Address.owner = Directory.service_node.owner;
      pname = Processes.Service.name
    }, Address.Root);

    ({
      Address.owner = Directory.broker_node.owner;
      pname = Processes.Broker.name
    }, Address.Root)
  |]
  

module Oracle =
struct

  module I =
    struct
      
      type t = unit
      [@@deriving yojson,eq,show]

      let serialize _ = failwith "useless"

      let deserialize _ _ = ()

    end

  module O =
  struct
    
    type t = [`leader of unit]
    [@@deriving eq,show]

    let serialize v =
      Bytes.to_string (Marshal.to_bytes v [])

    let deserialize _ _ = failwith "useless"

  end

  type state = unit

  let show_state i = "()"

  let name = Process.Name.atom "oracle"
    
  let rec main_loop state =
    Process.without_input
      (Lwt_unix.sleep 1.0;%lwt
       let dst = if Random.bool () then face.(0) else face.(1) in
       let output = { Process.Address.msg = `leader (); dests = [dst] } in
       Lwt_log.log_f ~level:Info "elected as leader: %s" (Process.Address.show (fst dst));%lwt
       Process.continue_with ~output state main_loop)

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
  OracleNode.start_dynamic
    ~listening:Directory.oracle
    ~network_map:Directory.network_map
