open Batteries
open Lwt.Infix
open Zmq

open Types

module LwtSocket = Zmq_lwt.Socket
module Json = Yojson.Safe


(*
  thread1: receive/ack/transition -> write via ipc to thread2 (pair)
  thread2: read on pair, write and get ack
*)

module Make(P : Protocol_Sig) =
struct

  type message =
    | Void of { nonce : int64 }
    | Json of { json : json; nonce : int64 }
    | Ack  of { nonce : int64 }
      
  type ('in_msg, 'out_msg) t =
    {
      ingoing  : [`Rep] Socket.t list; (* recv; send *)
      outgoing : [`Req] Socket.t list; (* send; recv *)
      state    : P.state
    }

  let get_nonce = function
    | Void { nonce }
    | Json { nonce }
    | Ack  { nonce } -> nonce

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
      failwith "huxiang/node/input: input message too short"
    else
      let headr = String.head str 4 in
      let nonce = int64_of_bytes (String.sub str 4 8) in
      let tail  = String.tail str 12 in
      match headr with
      | "void" ->
        Void { nonce }
      | "json" ->
        let json = Json.from_string tail in
        Json { json; nonce }
      | "ackn" ->
        Ack { nonce }
      | _ ->
        failwith "huxiang/node/input: incorrect header"

  let output msg =
    match msg with
    | Void { nonce } ->
      "void"^(bytes_of_int64 nonce)
    | Json { json; nonce } ->
      "json"^(bytes_of_int64 nonce)^(Json.to_string json)
    | Ack { nonce } ->
      "ackn"^(bytes_of_int64 nonce)

  let read_and_ack provider =
    let str = Socket.recv provider in
    let msg = input str in
    match msg with
    | Void { nonce }
    | Json { nonce } ->
      let reply = Ack { nonce } in
      Socket.send provider (output reply);
      msg   
    | Ack _ ->
      failwith "huxiang/node/read_and_ack: wrong message"

  let write_and_get_acked msg (consumer : [`Req] Socket.t) =
    Socket.send consumer (output msg);
    Printf.eprintf "output message sent, waiting for ack\n%!";
    let str = Socket.recv consumer in
    Printf.eprintf "ack received\n";
    let msg = input str in
    match msg with
    | Ack { nonce } ->
      if not (Int64.equal nonce (get_nonce msg)) then
        failwith "huxiang/node/write_and_get_acked: wrong nonce"
      else
        ()
    | _ ->  
      failwith "huxiang/node/write_and_get_acked: wrong message"
        
  let read_from_ingoing ingoing =
    let input_messages =
      List.map read_and_ack ingoing
    in
    List.fold_left (fun acc msg ->
        match msg with
        | Ack _ | Void _ -> acc
        | Json { json } ->
          (P.I.from_json json) :: acc
      ) [] input_messages

  let write_to_outgoing msgs nonce (outgoing : [`Req] Socket.t list) =
    List.iter (fun msg ->
        let m = Json { json = P.O.to_json msg; nonce } in
        List.iter (write_and_get_acked m) outgoing
      ) msgs

  let evolve_state in_msgs state =
    List.fold_left (fun (state, out_msgs) in_msg ->
        let st, out_opt = P.transition state in_msg in
        match out_opt with
        | None   -> (st, out_msgs)
        | Some m -> (st, m :: out_msgs)
      ) (state, []) in_msgs
  
  let thread { ingoing; outgoing; state } =
    let rec loop state nonce =
      Printf.eprintf "loop entered\n";
      let in_msgs = read_from_ingoing ingoing in
      let st, out = evolve_state in_msgs state in
      write_to_outgoing out nonce outgoing;
      loop state Int64.(nonce + one)
    in
    match P.initial_message with
    | None ->
      loop state 0L
    | Some msg ->
      (Printf.eprintf "writing on outgoing port\n%!";
       write_to_outgoing [msg] 0L outgoing;
       Printf.eprintf "initial message sent\n%!";
       loop state 1L)

  let start ~ingoing ~outgoing =
    let ctx       = Context.create () in
    let ingoing =
      List.map (fun prov_addr ->
          let sck = Socket.(create ctx rep) in
          (try Socket.connect sck prov_addr with _ -> failwith "rep bind");
          sck
        ) ingoing
    in
    let outgoing =
      List.map (fun cons_addr ->
          let sck = Socket.(create ctx req) in
          (try Socket.bind sck cons_addr with _ -> failwith "req bind");
          sck
        ) outgoing
    in
    let record = { ingoing; outgoing; state = P.initial_state } in
    Printf.printf "Starting node\n%!";
    thread record

end
