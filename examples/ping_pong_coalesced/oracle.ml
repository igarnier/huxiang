open Huxiang.Types
open Huxiang

let face =
  let open Process in
  [|
    ({
      Address.owner = Directory.ping_node.owner;
      pname = Processes.PingPongForPing.name
    }, Address.Root);
    ({
      Address.owner = Directory.pong_node.owner;
      pname = Processes.PingPongForPong.name
    }, Address.Root)
  |]
  

module Oracle =
struct

  module I =
    struct
      
      type t = unit
      [@@deriving yojson,eq,show]

      let deserialize _ _ = ()

    end

  module O =
  struct
    
    type t = [`leader of unit]
    [@@deriving eq,show]

    let serialize v =
      Bytes.to_string (Marshal.to_bytes v [])

  end

  type state = unit

  let show_state i = "()"

  let name = Process.Name.atom "oracle"
    
  let rec main_loop state =
    Process.without_input
      (Lwt_unix.sleep 3.0;%lwt
       let dst = if Random.bool () then face.(0) else face.(1) in
       let output = Process.(`leader () @. dst) in
       let dst_name = Process.Address.show (fst dst) in
       Lwt_log.info_f "elected as leader: %s" dst_name;%lwt
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
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  OracleNode.start_dynamic
    ~listening:Directory.oracle
    ~network_map:Directory.network_map
