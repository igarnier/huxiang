open Batteries
open Zmq

module LwtSocket = Zmq_lwt.Socket

type network_map = Address.t -> string

open Bin_prot.Std

type message =
  | Msg of {
      msg : NetProcess.Input.t;
      uid : int64
    }
  | Ack of { uid : int64 }
[@@deriving bin_io]

let message_to_bytes msg =
  let buf   = Bin_prot.Utils.bin_dump bin_writer_message msg in
  Utils.buffer_to_bytes buf

let message_from_bytes bytes =
  let buffer = Utils.bytes_to_buffer bytes in
  bin_reader_message.read buffer ~pos_ref:(ref 0)

type routing =
  | Dynamic of (Address.t -> [`Req] LwtSocket.t)

module Make(P : NetProcess.S) =
struct
      
  type t =
    {
      ingoing  : [`Rep] LwtSocket.t; (* recv msg; send ack *)
      routing  : routing;
      mqueue   : NetProcess.output Lwt_mvar.t;
      process  : P.state NetProcess.t;
      skey     : Crypto.Secret.t;
      pkey     : Crypto.Public.t
    }

  let get_uid = function
    | Msg { uid; _ }
    | Ack { uid } -> uid
    
  let print_msg msg =
    match msg with
    | Msg { uid; _ } ->
      Printf.sprintf "msg(%Ld)" uid
    | Ack { uid } ->
      Printf.sprintf "ack(%Ld)" uid
  
  let lwt_fail fname msg =
    Lwt.fail_with @@ fname^": "^msg

  let lwt_debug fname msg =
    Lwt_log.debug_f "%s: %s" fname msg

  let input str =
    let bytes = Bytes.of_string str in
    Lwt.return (message_from_bytes bytes)
          
  let output msg =
    let bytes = message_to_bytes msg in
    Bytes.to_string bytes

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
          let module Cred : Crypto.Credentials = 
          struct 
            let public_key = pkey
            let secret_key = skey
          end
          in
          let open NetProcess in
          let data = Crypto.Signed.pack msg (module Types.HuxiangBytes) (module Cred) in
          Input.Signed { data }
        in
        write_and_get_acked (Msg { uid; msg }) socket
      ) dests
        
  let reader_thread { ingoing; mqueue; process; _ } =
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
               "error caught in Input transition: "^(Printexc.to_string exn) 
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
               "error caught in NoInput transition: "^(Printexc.to_string exn)
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

  let writer_thread { routing; mqueue; skey; pkey; _ } =
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
