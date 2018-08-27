open Batteries
open Zmq

module LwtSocket = Zmq_lwt.Socket
module Json = Yojson.Safe


module Make(P : NetProcess.S) =
struct

  type network_map = Address.t -> string

  type frame = NetProcess.input

  let frame_to_bytes (uid, frame) =
    let open NetProcess in
    Marshal.to_bytes (uid, frame.Input.data) []

  let frame_of_bytes bytes =
    let open NetProcess in
    let (uid, data, route_bytes) :
      int64 * NetProcess.Input.data * Bytes.t =
      Marshal.from_bytes bytes 0
    in
    let frame = { Input.data } in
    (uid, frame)

  type message =
    | Msg of {
        msg : frame;
        uid : int64
      }
    | Ack of { uid : int64 }
              
  type routing =
    | Dynamic of (Address.t -> [`Req] LwtSocket.t)
      
  type t =
    {
      ingoing  : [`Rep] LwtSocket.t; (* recv; send *)
      routing  : routing;
      mqueue   : NetProcess.output Lwt_mvar.t;
      process  : P.state NetProcess.t;
      skey     : Crypto.Secret.t;
      pkey     : Crypto.Public.t
    }

  let get_uid = function
    | Msg { uid }
    | Ack { uid } -> uid
    
  let print_msg msg =
    match msg with
    | Msg { msg; uid } ->
      let open NetProcess in
      Printf.sprintf "msg(%Ld)" uid
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

  (* let serialize_message (msg : O.t) (path : Address.access_path) =
   *   let msgbytes = O.serialize msg in
   *   Bytes.to_string (Marshal.to_bytes (path, msgbytes) [])
   * 
   * let deserialize_message (bytes : Bytes.t) =
   *   let path, msgbytes =
   *     (Marshal.from_bytes bytes 0 : Address.access_path * Bytes.t)
   *   in
   *   try%lwt Lwt.return (I.deserialize (Bytes.to_string msgbytes) path)
   *   with
   *   | exn ->
   *     Lwt.fail_with @@ "huxiang/node/deserialize_message: error caught: "^
   *                      (Printexc.to_string exn) *)
  let lwt_fail fname msg =
    Lwt.fail_with @@ fname^": "^msg

  let lwt_debug fname msg =
    Lwt_log.debug_f "%s: %s" fname msg

  let input str =
    let fname = "huxiang/node/input" in
    if String.length str < 4 then
      lwt_fail fname "input message too short"
    else
      let headr = String.head str 4 in
      match headr with
      | "mesg" ->
        let tail  = String.tail str 4 in
        let (uid, msg) = frame_of_bytes (Bytes.of_string tail) in
        Lwt.return (Msg { msg; uid })
      | "ackn" ->
        let tail  = String.tail str 4 in
        Lwt.return (Ack { uid = int64_of_bytes tail })
      | _ ->
        lwt_fail fname "incorrect header"
          
  let output msg =
    match msg with
    | Msg { msg; uid } ->
      let bytes = frame_to_bytes (uid, msg) in
      let str   = Bytes.to_string bytes in
      "mesg"^str
    | Ack { uid } ->
      "ackn"^(bytes_of_int64 uid)

  let read_and_ack provider =
    let fname = "huxiang/node/read_and_ack" in
    let%lwt str = LwtSocket.recv provider in
    let%lwt msg = input str in
    lwt_debug fname ("read message "^(print_msg msg));%lwt
    match msg with
    | Msg { msg; uid } ->
      let reply = Ack { uid } in
      LwtSocket.send provider (output reply);%lwt
      Lwt.return msg
    |  _ ->
      lwt_fail fname "wrong message"

  let write_and_get_acked out_msg (consumer : [`Req] LwtSocket.t) =
    let fname = "huxiang/node/write_and_get_acked" in
    let%lwt serialized =
      try%lwt Lwt.return (output out_msg)
      with exn ->
        let message = 
          "error caught during serialization: "^(Printexc.to_string exn) 
        in
        lwt_debug fname message;%lwt
        Lwt.fail exn
    in
    LwtSocket.send consumer serialized;%lwt
    lwt_debug fname ("message sent, waiting for ack: "^(print_msg out_msg));%lwt
    let%lwt str = LwtSocket.recv consumer in
    lwt_debug fname "ack received";%lwt
    let%lwt msg = input str in
    match msg with
    | Ack { uid } ->
      if not (Int64.equal uid (get_uid msg)) then
        lwt_fail fname "wrong uid"
      else
        Lwt.return ()
    | _ ->  
      lwt_fail fname "wrong message"
        
  let read_from_ingoing ingoing = read_and_ack ingoing

  let write_to_outgoing { Address.msg; dests } uid (Dynamic table) skey pkey =
    let fname = "huxiang/node/write_to_outgoing" in
    Lwt_list.iter_p (fun address ->
        let%lwt socket = 
          try%lwt Lwt.return (table address)
          with
          | exn ->
            let message = 
              "error in network table computation: "^(Printexc.to_string exn)
            in
            lwt_debug fname message;%lwt
            Lwt.fail exn
        in
        let msg  = 
          let open NetProcess in
          { Input.data = Signed { data = Crypto.sign skey msg; pkey } }
        in
        write_and_get_acked (Msg { uid; msg }) socket
      ) dests
        
  let reader_thread { ingoing; mqueue; process } =
    let fname = "huxiang/node/reader_thread" in
    let rec loop process =
      lwt_debug fname "loop entered";%lwt
      let continue out next = (* factorised continuation *)
        (match out with
         | None -> Lwt.return ()
         | Some out ->
           Lwt_mvar.put mqueue out;%lwt
           lwt_debug fname "put"
        );%lwt
        loop next
      in
      (* TODO: Here we need to parameterize the node by a scheduler /!!!!\*)
      match Process.evolve process with
      | (Input transition) :: _ ->
        lwt_debug fname "Input transition";%lwt
        (let%lwt msg = read_from_ingoing ingoing in
         lwt_debug fname "read";%lwt
         let%lwt { Process.output; next } =
           try%lwt transition msg with
           | exn ->
             let message = 
               "error caught in evolve: "^(Printexc.to_string exn) 
             in
             lwt_debug fname message;%lwt
             Lwt.fail exn
         in
         continue output next
        )
      | (NoInput transition) :: _ ->
        lwt_debug fname "NoInput transition";%lwt
        let%lwt { Process.output; next } =
          try%lwt transition with
          | exn ->
             let message = 
               "error caught in evolve: "^(Printexc.to_string exn) 
             in
             lwt_debug fname message;%lwt
             Lwt.fail exn
        in
        continue output next
      | [] ->
        Lwt_log.log ~level:Info "reader: Stop state reached";%lwt
        Lwt.return ()
    in
    loop process

  let writer_thread { routing; mqueue; skey; pkey } =
    let fname = "huxiang/node/writer_thread" in
    let rec loop uid =
      lwt_debug fname "loop entered";%lwt
      let%lwt msg = Lwt_mvar.take mqueue in
      lwt_debug fname "taken";%lwt
      write_to_outgoing msg uid routing skey pkey;%lwt
      lwt_debug fname "written";%lwt
      loop Int64.(uid + one)
    in
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

  let start ~listening ~network_map ~skey ~pkey =
    with_context (fun ctx ->
        let ingoing =
          let sck = Socket.(create ctx rep) in
          (try Socket.bind sck listening with
           | exn ->
             let s = Printexc.to_string exn in
             failwith @@
             "Could not connect to address "^listening^": "^
             "error "^s^" caught");
          LwtSocket.of_socket sck
        in
        let table = Hashtbl.create 30 in
        let record =
          {
            ingoing;
            routing = Dynamic (fun x -> get_socket ctx table (network_map x));
            mqueue  = Lwt_mvar.create_empty ();
            process = P.thread;
            skey;
            pkey
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
               Lwt_log.log ~level:Info "Shutting node down!";%lwt
               Lwt.return (close ingoing outgoing)
            )
        in        
        Lwt_main.run program
      )

end
