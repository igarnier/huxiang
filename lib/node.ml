open Batteries
open Zmq

open Types

module LwtSocket = Zmq_lwt.Socket
module Json = Yojson.Safe

module Make(P : Process.S) =
struct

  type address     = string
  type addresses   = address list

  type out_dispatch = P.O.t -> address list

  type message =
    | Json of { json : json; uid : int64 }
    | Ack  of { uid : int64 }
              
  type routing =
    | Mcast   of [`Req] LwtSocket.t list
    | Dynamic of (P.O.t -> [`Req] LwtSocket.t list)
      
  type t =
    {
      ingoing  : [`Rep] LwtSocket.t; (* recv; send *)
      routing  : routing;
      mqueue   : P.O.t Lwt_mvar.t;
      process  : (module Process.S with type I.t = P.I.t and type O.t = P.O.t)
    }
  
  let get_uid = function
    | Json { uid }
    | Ack  { uid } -> uid

  let print_msg msg =
    match msg with
    | Json { json; uid } ->
      Printf.sprintf "json(%s/%Ld)" (Json.to_string json) uid
    | Ack { uid } ->
      Printf.sprintf "ack(%Ld)" uid
  
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
      let uid = int64_of_bytes (String.sub str 4 8) in
      let tail  = String.tail str 12 in
      match headr with
      | "json" ->
        let json = Json.from_string tail in
        Lwt.return (Json { json; uid })
      | "ackn" ->
        Lwt.return (Ack { uid })
      | _ ->
        Lwt.fail (Failure "huxiang/node/input: incorrect header")

  let output msg =
    match msg with
    | Json { json; uid } ->
      "json"^(bytes_of_int64 uid)^(Json.to_string json)
    | Ack { uid } ->
      "ackn"^(bytes_of_int64 uid)

  let read_and_ack provider =
    let%lwt str = LwtSocket.recv provider in
    let%lwt msg = input str in
    Lwt_log.log_f ~level:Debug "reader: read message %s" (print_msg msg);%lwt
    match msg with
    | Json { uid; json } ->
      let reply = Ack { uid } in      
      LwtSocket.send provider (output reply);%lwt
      Lwt.return json 
    | Ack _ ->
      Lwt.fail_with "huxiang/node/read_and_ack: wrong message"

  let write_and_get_acked msg (consumer : [`Req] LwtSocket.t) =
    LwtSocket.send consumer (output msg);%lwt
    Lwt_log.log_f ~level:Debug "writer: message %s sent, waiting for ack" (print_msg msg);%lwt
    let%lwt str = LwtSocket.recv consumer in
    Lwt_log.log ~level:Debug "writer: ack received";%lwt
    let%lwt msg = input str in
    match msg with
    | Ack { uid } ->
      if not (Int64.equal uid (get_uid msg)) then
        Lwt.fail_with "huxiang/node/write_and_get_acked: wrong uid"
      else
        Lwt.return ()
    | _ ->  
      Lwt.fail_with "huxiang/node/write_and_get_acked: wrong message"
        
  let read_from_ingoing ingoing =
    let%lwt json = read_and_ack ingoing in
    match P.I.of_yojson json with
    | Ok j ->
      Lwt.return j
    | Error e ->
      Lwt.fail_with @@
      "huxiang/node/read_from_ingoing: error in json deserialization: "^e

  let write_to_outgoing msg uid routing =
    let m = Json { json = P.O.to_yojson msg; uid } in
    let outgoing =
      match routing with
      | Mcast outgoing -> outgoing
      | Dynamic table  -> table msg
    in
    Lwt_list.iter_p (write_and_get_acked m) outgoing
        
  let reader_thread { ingoing; mqueue; process } =
    let rec loop process =
      Lwt_log.log ~level:Debug "reader: loop entered";%lwt
      let continue out next = (* factorised continuation *)
        Lwt_log.log ~level:Debug "reader: evolved";%lwt
        (match out with
         | None -> Lwt.return ()
         | Some out ->
           Lwt_mvar.put mqueue out;%lwt
           Lwt_log.log ~level:Debug "reader: put"
        );%lwt
        loop next
      in
      match Process.evolve process with
      | Input transition ->
        (let%lwt in_msg = read_from_ingoing ingoing in
         Lwt_log.log ~level:Debug "reader: read";%lwt
         let%lwt out, next =
           try%lwt transition in_msg with
           | exn ->
             Lwt_log.log_f ~level:Debug "reader: error caught in evolve: %s" (Printexc.to_string exn);%lwt
             Lwt.fail exn
         in
         continue out next
        )
      | NoInput transition ->
        let%lwt out, next =
          try%lwt transition with
          | exn ->
            Lwt_log.log_f ~level:Debug "reader: error caught in evolve: %s" (Printexc.to_string exn);%lwt
            Lwt.fail exn
        in
        continue out next
      | Stop ->
        Lwt.return ()
    in
    loop (module P)

  let writer_thread { routing; mqueue } =
    let rec loop uid =
      Lwt_log.log ~level:Debug "writer: loop entered";%lwt
      let%lwt msg = Lwt_mvar.take mqueue in
      Lwt_log.log ~level:Debug "writer: taken";%lwt
      write_to_outgoing msg uid routing;%lwt
      Lwt_log.log ~level:Debug "writer: written";%lwt
      loop Int64.(uid + one)
    in
    (* match P.initial_message with
     * | None ->
     *   loop 0L
     * | Some msg ->
     *   (Lwt_log.log ~level:Debug "writing on outgoing port";%lwt
     *    write_to_outgoing msg 0L routing;%lwt
     *    Lwt_log.log ~level:Debug "initial message sent";%lwt *)
    loop 0L

  let with_context f =
    let c = Context.create () in
    let r = f c in
    Context.terminate c;
    r

  let close ingoing outgoing =
    Socket.close (LwtSocket.to_socket ingoing);
    List.iter (fun s ->
        let s = LwtSocket.to_socket s in
        Socket.close s
      ) outgoing

  let start_mcast ~listening ~outgoing =
    with_context (fun ctx ->

        let ingoing =
          let sck = Socket.(create ctx rep) in
          Socket.bind sck listening;
          LwtSocket.of_socket sck
        in
        let outgoing =
          List.map (fun cons_addr ->
              let sck = Socket.(create ctx req) in
              Socket.connect sck cons_addr;
              LwtSocket.of_socket sck
            ) outgoing
        in
        let routing = Mcast outgoing in
        let record =
          {
            ingoing;
            routing;
            mqueue  = Lwt_mvar.create_empty ();
            process = (module P)
          }
        in
        let program =
          Lwt.finalize
            (fun () ->
               Lwt_log.log ~level:Info "Starting node!";%lwt
               Lwt.join [ reader_thread record;
                          writer_thread record ])
            (fun () -> Lwt.return (close ingoing outgoing))
        in        
        Lwt_main.run program

      )
      
  let get_socket ctx table address =
    match Hashtbl.find_option table address with
    | None ->
      let sck = Socket.(create ctx req) in
      (try Socket.connect sck address with
       | exn ->
         let s = Printexc.to_string exn in
         failwith @@
         "huxiang/node/get_socket: exception "^s^
         " raised while binding addr "^
         address);
      let sck = LwtSocket.of_socket sck in
      Hashtbl.add table address sck;
      sck
    | Some sck ->
      sck

  (* let check_uniq l =
   *   List.length (List.sort_uniq String.compare l) = (List.length l) *)

  let start_dynamic ~listening ~out_dispatch =
    with_context (fun ctx ->
        let ingoing =
          let sck = Socket.(create ctx rep) in
          (try Socket.bind sck listening
           with
           | exn ->
             let s = Printexc.to_string exn in
             failwith @@
             "Could not connect to address "^listening^": "^
             "error "^s^" caught");
          LwtSocket.of_socket sck
        in
        let table = Hashtbl.create 30 in
        let routing =
          Dynamic (fun msg ->
              let addresses = out_dispatch msg in
              let sockets   = List.map (get_socket ctx table) addresses in
              sockets
            )
        in
        let record =
          {
            ingoing;
            routing;
            mqueue  = Lwt_mvar.create_empty ();
            process = (module P)
          }
        in
        let program =
          Lwt.finalize
            (fun () ->
               Lwt_log.log ~level:Info "Starting node!";%lwt
               Lwt.join [ reader_thread record;
                          writer_thread record ])
            (fun () ->
               let outgoing = Hashtbl.values table in
               let outgoing = List.of_enum outgoing in
               Lwt.return (close ingoing outgoing)
            )
        in        
        Lwt_main.run program
      )

end
