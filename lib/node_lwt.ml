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
    Lwt_io.printf "reader: read message %s" (print_msg msg);%lwt
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
    Lwt_io.printf "writer: output message %s sent, waiting for ack\n" (print_msg msg);%lwt
    let%lwt str = LwtSocket.recv consumer in
    Lwt_io.print "writer: ack received\n";%lwt
    let%lwt msg = input str in
    Lwt_io.print "writer: input parsed\n";%lwt
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
      Lwt_io.print "reader: loop entered\n";%lwt
      let%lwt in_msgs = read_from_ingoing ingoing in
      Lwt_io.print "reader: read\n";%lwt
      let%lwt st, out = 
        try%lwt evolve_state in_msgs state
        with
        | exn ->
          Lwt_io.printf "reader: error caught in evolve: %s\n" (Printexc.to_string exn);%lwt
          Lwt.fail exn
      in
      Lwt_io.print "reader: evolved\n";%lwt
      Lwt_mvar.put mqueue out;%lwt
      Lwt_io.print "reader: put\n";%lwt
      loop st
    in
    loop state

  let writer_thread { outgoing; mqueue } =
    let rec loop nonce =
      Lwt_io.print "writer: loop entered\n";%lwt
      let%lwt msgs = Lwt_mvar.take mqueue in
      Lwt_io.print "writer: taken\n";%lwt
      write_to_outgoing msgs nonce outgoing;%lwt
      Lwt_io.print "writer: written\n";%lwt
      loop Int64.(nonce + one)
    in
    let nonce = 0L in
    let msg   = P.initial_message in
    Lwt_io.print "writing on outgoing port\n";%lwt
    write_to_outgoing [msg] nonce outgoing;%lwt
    Lwt_io.print "initial message sent\n";%lwt
    loop 1L

  let start ~ingoing ~outgoing =
    let ctx       = Context.create () in
    let ingoing =
      List.map (fun prov_addr ->
          let sck = Socket.(create ctx rep) in
          (try Socket.connect sck prov_addr with _ -> failwith "rep bind");
          LwtSocket.of_socket sck
        ) ingoing
    in
    let outgoing =
      List.map (fun cons_addr ->
          let sck = Socket.(create ctx req) in
          (try Socket.bind sck cons_addr with _ -> failwith "req bind");
          LwtSocket.of_socket sck
        ) outgoing
    in
    let record = { ingoing; outgoing;
                   state = P.initial_state;
                   mqueue = Lwt_mvar.create_empty () } in
    Printf.printf "Starting node\n%!";
    Lwt_main.run (Lwt.join [ reader_thread record;
                             writer_thread record ])

end
