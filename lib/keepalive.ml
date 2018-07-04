open Batteries
open Lwt.Infix

module Json = Yojson.Safe

type status =
  | LinkAvailable
  | AwaitAck of int64

type event =
  | Message of Transport.message
  | Timeout

type ('state, 'in_msg, 'out_msg) internal_state =
  {
    socket        : Lwt_unix.file_descr;
    state         : 'state Lwt_mvar.t;
    status        : status;
    nonce         : int64;
    keepalive_dt  : float; (* in seconds *)
    timeouts      : int;   (* consecutive timeouts on an expected Ack *)
    in_from_json  : Json.json -> 'in_msg;
    in_to_json    : 'in_msg -> Json.json;
    out_to_json   : 'out_msg -> Json.json;
    transition    : 'state -> 'in_msg -> ('state * 'out_msg option) Lwt.t;
    log_callback  : string -> unit Lwt.t
  }

let rec loop state =
  let%lwt event =
    Lwt.pick
      [
        (Transport.read_message state.socket >>= fun msg -> Lwt.return (Message msg));
        (Lwt_unix.sleep state.keepalive_dt;%lwt Lwt.return Timeout)
      ]
  in
  match event, state.status with
  | Timeout, LinkAvailable ->
    (* send a Ping *)
    state.log_callback ("keepalive(timeout): -> ping "^(Int64.to_string state.nonce));%lwt
    Transport.write_message state.socket (Transport.Ping state.nonce);%lwt
    loop  { state with status = AwaitAck state.nonce }
  | Timeout, AwaitAck _ ->
    state.log_callback "keepalive(await_ack)";%lwt
    if state.timeouts > 10 then
      Lwt.fail_with "huxiang: timeout on Ack - aborting\n"
    else
      loop { state with timeouts = state.timeouts + 1 }
  | Message m, AwaitAck i ->
    (match m with
     | Transport.Ack i' ->
       state.log_callback @@ "keepalive(await_ack): <- ack"^(Int64.to_string i');%lwt
       if Int64.equal i i' then
         loop { state with status = LinkAvailable; nonce = Int64.(state.nonce + 1L); timeouts = 0 }
       else
         Lwt.fail_with "huxiang: Ack has wrong nonce - aborting\n"
     | Transport.Ping _ ->
       Lwt.fail_with "huxiang: received Ping instead of Ack - aborting\n"
     | Transport.Json json ->
       state.log_callback @@ "keepalive(await_ack): <- json";%lwt
       let message = state.in_from_json json in
       handle_protocol_message message state
    )      
  | Message m, LinkAvailable ->
    (match m with
     | Transport.Json json ->
       state.log_callback @@ "keepalive(link_available): <- json";%lwt
       let message = state.in_from_json json in
       handle_protocol_message message state
     | Transport.Ping i ->       
       if i >= state.nonce then
         (state.log_callback @@ "keepalive(link_available): <- ping"^(Int64.to_string i);%lwt
          state.log_callback @@ "keepalive(link_available): -> ack"^(Int64.to_string i);%lwt
          Transport.write_message state.socket (Transport.Ack i);%lwt
          loop { state with nonce = i })
       else
         Lwt.fail_with "huxiang: incoming ping has outdated nonce - aborting\n"
     | Transport.Ack _ ->
       Lwt.fail_with "huxiang: unexpected message - aborting\n")

and handle_protocol_message message state =
  state.log_callback ("read message: "^(Json.to_string (state.in_to_json message)));%lwt
  let%lwt cstate = Lwt_mvar.take state.state in
  let%lwt new_cstate, m_opt =
    try%lwt
      state.transition cstate message
    with 
    | exn -> 
      (Lwt_io.eprint "huxiang: error caught in P.transition\n";%lwt
       Lwt_mvar.put state.state cstate;%lwt
       Lwt.fail exn)
  in
  Lwt_mvar.put state.state new_cstate;%lwt
  (match m_opt with
   | None     -> Lwt.return ()
   | Some msg ->
     state.log_callback ("wrote message: "^(Json.to_string (state.out_to_json msg)));%lwt
     Transport.write_message state.socket (Transport.Json (state.out_to_json msg)));%lwt
  loop state
