open Batteries
open Zmq

(* Aux functions *)

let lwt_debug fname msg =
  Lwt_log.debug_f "%s: %s" fname msg

let lwt_info fname msg =
  Lwt_log.info_f "%s: %s" fname msg

let lwt_fail fname msg =
  Lwt.fail_with @@ fname^": "^msg

let lwt_fail_exn fname msg exn =
  let s   = Printexc.to_string exn in
  let err =
    Printf.sprintf "%s: exception %s raised (%s)" fname s msg
  in
  Lwt.fail_with err

(* let fail_exn fname msg exn =
 *   let s   = Printexc.to_string exn in
 *   let err =
 *     Printf.sprintf "%s: exception %s raised (%s)" fname s msg
 *   in
 *   failwith err *)




module LwtSocket = Zmq_lwt.Socket

type address = string
[@@deriving show]

let _ = show_address

type input_socket =
  | Subscribe  of address
  | ReliableIn of address
[@@deriving show]

type output_socket =
  | Publish     of address
  | ReliableOut of address
(* [@@deriving show] *)

type dest =
  | Addr of Address.t
  | Bcast

type network_map = dest -> output_socket

open Bin_prot.Std

type message =
  | Msg of {
      msg : NetProcess.Input.t;
      uid : int64
    }
[@@deriving bin_io, show]

(* serialise message *)
let message_to_bytes msg =
  let buf = Bin_prot.Utils.bin_dump bin_writer_message msg in
  Utils.buffer_to_bytes buf

(* deserialise message *)
let message_from_bytes bytes =
  let buffer = Utils.bytes_to_buffer bytes in
  bin_reader_message.read buffer ~pos_ref:(ref 0)

type routing =
  | Dynamic of (dest -> [`Dealer | `Pub] LwtSocket.t)

type node_state =
  {
    ingoing  : [`Dealer | `Sub] LwtSocket.t list;
    routing  : routing;
    iqueue   : NetProcess.input list Lwt_mvar.t;
    oqueue   : NetProcess.output Lwt_mvar.t;
    creds    : (module Crypto.Credentials)
  }

(* -------------------------------------------------------------------------- *)
(* Reader thread *)

let message_from_string (str : string) =
  let bytes = Bytes.of_string str in
  message_from_bytes bytes

let read_sockets sockets =
  let     reads  = List.map LwtSocket.recv sockets in  
  let%lwt result = Lwt.npick reads in
  let     result = 
    List.map (message_from_string %> (fun (Msg { msg; _}) -> msg)) result
  in
  Lwt.return result

let reader_thread { ingoing; iqueue; _ } =
  let fname = "huxiang/node/reader_thread" in
  let rec loop () =
    let%lwt messages = read_sockets ingoing in
    match messages with
    | [] ->
      lwt_debug fname "read no message";%lwt
      loop ()
    | _ ->
      lwt_debug fname "read some messages";%lwt
      let%lwt previous_msgs = Lwt_mvar.take iqueue in
      lwt_debug fname "put them in the iqueue";%lwt
      Lwt_mvar.put iqueue (previous_msgs @ messages);%lwt
      loop ()
  in
  loop ()

(* Writer thread *)

let message_to_string msg =
  let bytes = message_to_bytes msg in
  Bytes.to_string bytes

let write_socket msg (consumer : 'a LwtSocket.t) =
  let fname = "huxiang/node/write_socket" in
  let serialized = message_to_string msg in
  LwtSocket.send consumer serialized;%lwt
  lwt_debug fname ("message sent: "^(show_message msg))

let write_to_outgoing { Address.msg; dests } uid (Dynamic table) creds =
  (* let fname = "huxiang/node/write_to_outgoing" in *)
  let data = Crypto.Signed.pack msg (module Types.HuxiangBytes) creds in
  let msg  = Msg { msg = NetProcess.Input.Signed { data }; uid } in
  match dests with
  | [] ->
    (* no dests -> bcast *)
    let socket = table Bcast in
    write_socket msg socket
  | _ ->
    Lwt_list.iter_p (fun address ->
        let socket = table (Addr address) in
        write_socket msg socket
      ) dests

let writer_thread { routing; oqueue; creds; _ } =
  let rec loop uid =
    let%lwt msg = Lwt_mvar.take oqueue in
    write_to_outgoing msg uid routing creds;%lwt
    loop Int64.(uid + one)
  in
  loop Int64.zero
 

module Make(P : NetProcess.S) =
struct

  type t =
    {
      node_state : node_state;
      process    : P.state NetProcess.t
    }
      
  let is_input_available { node_state = { iqueue; _ }; _ } =
    let%lwt inputs = Lwt_mvar.take iqueue in
    let result = not (List.is_empty inputs) in
    Lwt_mvar.put iqueue inputs;%lwt
    Lwt.return result

  let take_one_input { node_state = { iqueue; _ }; _ } =
    let fname = "huxiang/node/take_one_input" in
    let%lwt inputs = Lwt_mvar.take iqueue in
    match inputs with
    | [] ->
      Lwt_mvar.put iqueue [];%lwt
      lwt_fail fname "queue should not be empty, bug found"
    | input :: tl ->
      Lwt_mvar.put iqueue tl;%lwt
      Lwt.return input

  let process_thread state =
    let fname = "huxiang/node/process_thread" in
    let rec loop process =
      lwt_debug fname ("entering loop with state"^(P.show_state process.Process.state));%lwt
      let transitions = Process.evolve process in
      match transitions with
      | [] ->
        lwt_info fname "final state reached";%lwt
        Lwt.return ()
      | _ ->
        begin
          let%lwt input_present = is_input_available state in
          let transitions =
            (* filter out inputful transitions when there are no
               inputs available. *)
            List.filter (function
                | Process.Input _ -> input_present
                | _               -> true
              ) transitions
          in
          match transitions with
          | [] ->
            (* No transitions can be played now. 
               TODO: we should wake up this thread only when some new input
               arrives. *)
            lwt_debug fname "no playable transition";%lwt
            Lwt.pause ();%lwt
            loop process
          | _ -> 
            begin
              (* pick a transition at random, play it *)
              let tr = Random.choice (List.enum transitions) in
              match tr with
              | Input transition ->
                lwt_debug fname "playing Input transition";%lwt
                let%lwt msg = take_one_input state in
                lwt_debug fname "took one input";%lwt
                let%lwt { Process.output; next } =
                  try%lwt transition msg with
                  | exn ->
                    lwt_fail_exn fname "error caught in Input transition" exn
                in
                lwt_debug fname "played successfully";%lwt
                continue output next
              | NoInput transition ->
                lwt_debug fname "playing NoInput transition";%lwt
                let%lwt { Process.output; next } =
                  try%lwt transition with
                  | exn ->
                    lwt_fail_exn fname "error caught in NoInput transition" exn
                in
                continue output next
            end
        end
    and continue out next = (* factorised continuation *)
      (match out with
       | None     -> 
         Lwt.return ()
       | Some out -> 
         Lwt_mvar.put state.node_state.oqueue out
      );%lwt
      loop next
    in
    loop state.process

  let with_context f =
    let c = Context.create () in
    let r = f c in
    Context.terminate c;
    r

  let close ingoing outgoing =
    List.iter (fun x -> Socket.close (LwtSocket.to_socket x)) ingoing;
    List.iter (fun s ->
        let s = LwtSocket.to_socket s in
        Socket.close s
      ) outgoing
      
  let create_in_socket ctx input_socket =
    let open Socket in
    match input_socket with 
    | Subscribe addr ->
      let sck = create ctx sub in
      Socket.bind sck addr;
      Socket.subscribe sck "";
      LwtSocket.of_socket sck
    | ReliableIn addr ->
      let sck = create ctx dealer in
      Socket.bind sck addr;
      LwtSocket.of_socket sck

  let create_out_socket ctx output_socket =
    let open Socket in
    match output_socket with 
    | Publish addr ->
      let sck = create ctx pub in
      Socket.connect sck addr;
      LwtSocket.of_socket sck
    | ReliableOut addr ->
      let sck = create ctx dealer in
      Socket.connect sck addr;
      LwtSocket.of_socket sck

  let get_socket ctx table output_socket =
    (* let fname = "huxiang/node/get_socket" in *)
    match Hashtbl.find_option table output_socket with
    | None ->
      let sck = create_out_socket ctx output_socket in
      Hashtbl.add table output_socket sck;
      sck
    | Some sck ->
      sck
         
  let start
      ~(listening : input_socket list)
      ~(network_map : network_map)
      ~(credentials : (module Crypto.Credentials)) =
    (* let fname = "huxiang/node/start" in *)
    let module Cred = (val credentials : Crypto.Credentials) in
    with_context (fun ctx ->
        let ingoing = List.map (create_in_socket ctx) listening in
        let table   = Hashtbl.create 30 in
        let node_state = {
          ingoing;
          routing = Dynamic (fun x -> get_socket ctx table (network_map x));
          iqueue  = Lwt_mvar.create [];
          oqueue  = Lwt_mvar.create_empty ();
          creds   = credentials;
        } in
        let whole_state = { node_state; process = P.thread } in
        let program =
          Lwt.finalize
            (fun () ->
               Lwt_log.log ~level:Info "Starting node!";%lwt
               let in_sockets =
                 listening
                 |> List.map show_input_socket
                 |> String.concat ", "
               in
               Lwt_log.log_f ~level:Info "Listening to: %s" in_sockets;%lwt
               Lwt.join [ reader_thread node_state;
                          writer_thread node_state;
                          process_thread whole_state
                        ])
            (fun () ->
               let outgoing = Hashtbl.values table in
               let outgoing = List.of_enum outgoing in
               Lwt_log.log ~level:Info "Shutting node down!";%lwt
               Lwt.return (close ingoing outgoing)
            )
        in        
        Lwt_main.run program
      )

end
