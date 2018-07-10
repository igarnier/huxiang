open Batteries
open Zmq

open Types

module LwtSocket = Zmq_lwt.Socket
module Json = Yojson.Safe

module Make(P : ProtocolLwt_Sig) =
struct

  type message =
    | Void of { nonce : int64 }
    | Json of { json : json; nonce : int64 }
    | Ack  of { nonce : int64 }
      
  type ('in_msg, 'out_msg) t =
    {
      ingoing  : [`Rep] LwtSocket.t list; (* recv; send *)
      outgoing : [`Req] LwtSocket.t list; (* send; recv *)
      mqueue   : P.O.t list Lwt_mvar.t;
      state    : P.state
    }

  let get_nonce = function
    | Void { nonce }
    | Json { nonce }
    | Ack  { nonce } -> nonce

  let print_msg msg =
    match msg with
    | Void { nonce } -> 
      Printf.sprintf "void(%Ld)" nonce
    | Json { json; nonce } ->
      Printf.sprintf "json(%s/%Ld)" (Json.to_string json) nonce
    | Ack { nonce } ->
      Printf.sprintf "ack(%Ld)" nonce
  
  let bytes_of_int64 i =
    let io = IO.output_string () in
    IO.write_i64 io i;
    IO.close_out io

  let int64_of_bytes s =
    if String.length s <> 8 then
      failwith "huxiang/node/int64_of_string: length <> 8"
    else
      let io = IO.input_string s in
      IO.read_i64 io
    
  let input str =
    if String.length str < 12 then
      Lwt.fail_with "huxiang/node/input: input message too short"
    else
      let headr = String.head str 4 in
      let nonce = int64_of_bytes (String.sub str 4 8) in
      let tail  = String.tail str 12 in
      match headr with
      | "void" ->
        Lwt.return (Void { nonce })
      | "json" ->
        let json = Json.from_string tail in
        Lwt.return (Json { json; nonce })
      | "ackn" ->
        Lwt.return (Ack { nonce })
      | _ ->
        Lwt.fail (Failure "huxiang/node/input: incorrect header")

  let output msg =
    match msg with
    | Void { nonce } ->
      "void"^(bytes_of_int64 nonce)
    | Json { json; nonce } ->
      "json"^(bytes_of_int64 nonce)^(Json.to_string json)
    | Ack { nonce } ->
      "ackn"^(bytes_of_int64 nonce)

  let read_and_ack provider =
    let%lwt str = LwtSocket.recv provider in
    let%lwt msg = input str in
    Lwt_log.log_f ~level:Debug "reader: read message %s" (print_msg msg);%lwt
    match msg with
    | Void { nonce }
    | Json { nonce } ->
      let reply = Ack { nonce } in      
      LwtSocket.send provider (output reply);%lwt
      Lwt.return msg   
    | Ack _ ->
      Lwt.fail_with "huxiang/node/read_and_ack: wrong message"

  let write_and_get_acked msg (consumer : [`Req] LwtSocket.t) =
    LwtSocket.send consumer (output msg);%lwt
    Lwt_log.log_f ~level:Debug "writer: output message %s sent, waiting for ack" (print_msg msg);%lwt
    let%lwt str = LwtSocket.recv consumer in
    Lwt_log.log ~level:Debug "writer: ack received";%lwt
    let%lwt msg = input str in
    Lwt_log.log ~level:Debug "writer: input parsed";%lwt
    match msg with
    | Ack { nonce } ->
      if not (Int64.equal nonce (get_nonce msg)) then
        Lwt.fail_with "huxiang/node/write_and_get_acked: wrong nonce"
      else
        Lwt.return ()
    | _ ->  
      Lwt.fail_with "huxiang/node/write_and_get_acked: wrong message"
        
  let read_from_ingoing ingoing =
    let%lwt input_messages =
      Lwt_list.map_p read_and_ack ingoing
    in
    let messages =
      List.fold_left (fun acc msg ->
          match msg with
          | Ack _ | Void _ -> acc
          | Json { json } ->
            (P.I.from_json json) :: acc
        ) [] input_messages
    in
    Lwt.return messages

  let write_to_outgoing msgs nonce (outgoing : [`Req] LwtSocket.t list) =
    Lwt_list.iter_p (fun msg ->
        let m = Json { json = P.O.to_json msg; nonce } in
        Lwt_list.iter_p (write_and_get_acked m) outgoing
      ) msgs

  let evolve_state in_msgs state =
    Lwt_list.fold_left_s (fun (state, out_msgs) in_msg ->
        let%lwt st, out_opt = P.transition state in_msg in
        match out_opt with
        | None   -> Lwt.return (st, out_msgs)
        | Some m -> Lwt.return (st, m :: out_msgs)
      ) (state, []) in_msgs
  
  let reader_thread { ingoing; outgoing; mqueue; state } =
    let rec loop state =
      Lwt_log.log ~level:Debug "reader: loop entered";%lwt
      let%lwt in_msgs = read_from_ingoing ingoing in
      Lwt_log.log ~level:Debug "reader: read";%lwt
      let%lwt st, out = 
        try%lwt evolve_state in_msgs state
        with
        | exn ->
          Lwt_log.log_f ~level:Debug "reader: error caught in evolve: %s" (Printexc.to_string exn);%lwt
          Lwt.fail exn
      in
      Lwt_log.log ~level:Debug "reader: evolved";%lwt
      Lwt_mvar.put mqueue out;%lwt
      Lwt_log.log ~level:Debug "reader: put";%lwt
      loop st
    in
    loop state

  let writer_thread { outgoing; mqueue } =
    let rec loop nonce =
      Lwt_log.log ~level:Debug "writer: loop entered";%lwt
      let%lwt msgs = Lwt_mvar.take mqueue in
      Lwt_log.log ~level:Debug "writer: taken";%lwt
      write_to_outgoing msgs nonce outgoing;%lwt
      Lwt_log.log ~level:Debug "writer: written";%lwt
      loop Int64.(nonce + one)
    in
    match P.initial_message with
    | None ->
      loop 0L
    | Some msg ->
      (Lwt_log.log ~level:Debug "writing on outgoing port";%lwt
       write_to_outgoing [msg] 0L outgoing;%lwt
       Lwt_log.log ~level:Debug "initial message sent";%lwt
       loop 1L)

  let with_context f =
    let c = Context.create () in
    let r = f c in
    Context.terminate c;
    r

  let close { ingoing; outgoing } =
    List.iter (fun s ->
        let s = LwtSocket.to_socket s in
        Socket.close s
      ) ingoing;
    List.iter (fun s ->
        let s = LwtSocket.to_socket s in
        Socket.close s
      ) outgoing

  let start ~ingoing ~outgoing =
    with_context (fun ctx ->

        let ingoing =
          List.map (fun prov_addr ->
              let sck = Socket.(create ctx rep) in
              Socket.connect sck prov_addr;
              LwtSocket.of_socket sck
            ) ingoing
        in
        let outgoing =
          List.map (fun cons_addr ->
              let sck = Socket.(create ctx req) in
              Socket.bind sck cons_addr;
              LwtSocket.of_socket sck
            ) outgoing
        in
        let record = { ingoing; outgoing;
                       state = P.initial_state;
                       mqueue = Lwt_mvar.create_empty () } in
        let program =
          Lwt.finalize
            (fun () ->
               Lwt_log.log ~level:Info "Starting node!";%lwt
               Lwt.join [ reader_thread record;
                          writer_thread record ])
            (fun () -> Lwt.return (close record))
        in        
        Lwt_main.run program

      )
end
