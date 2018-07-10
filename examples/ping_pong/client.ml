open Batteries
open Huxiang.Types

module Protocol =
struct

  module I = Messages.ServerToClient
  module O = Messages.ClientToServer

  type status =
    | Unborn
    | Alive
    | Dead

  type state = {
    id     : O.client_id;
    status : status;
    nonce  : int
  }

  let initial_state =
    {
      id     = "TODO parameterize protocol by id";
      status = Unborn;
      nonce  = 0
    }

  let initial_message =
    Some (O.Hello { client_id = initial_state.id; nonce = initial_state.nonce })

  let alive id nonce =
    O.Alive { client_id = id; nonce }

  let dead_state =
    { initial_state with status = Dead; nonce = -1 }

  (* Assuming the initial message to Mother will be 
     Hello { client_id = initial_state.id; nonce = initial_state.nonce }.
     It would be good if that initial message was part of the protocol.
  *)
  let transition state in_msg =
    match state.status with
    | Dead   ->
      Lwt.return (dead_state, None)
    | Unborn ->
      (match in_msg with
       | I.ServerStartup ->
         Lwt.return (state, None)
       | I.Ack (O.Hello { client_id; nonce })
         when (client_id = initial_state.id && nonce = initial_state.nonce) ->
         Lwt_io.eprintf "ack (hello %s %d)\n" client_id nonce;%lwt
         let state = { state with
                       status = Alive;
                       nonce  = state.nonce + 1
                     } in
         Lwt.return (state, Some (alive state.id state.nonce))
       | _ ->
         Lwt_io.eprintf "client dead\n";%lwt
         Lwt.return (dead_state, None)
      )
    | Alive ->
      (match in_msg with
       | I.ServerStartup ->
         Lwt.return (state, None)       
       | I.Ack (O.Alive { client_id; nonce })           
         when client_id = state.id && nonce = state.nonce ->
         Lwt_io.eprintf "ack (alive %s %d)\n" client_id nonce;%lwt
         let state = { state with
                       nonce  = state.nonce + 1
                     } in
         Lwt.return (state, Some (alive state.id state.nonce))
       | _ ->
         Lwt_io.eprintf "client dead\n";%lwt
         Lwt.return (dead_state, None)
      )


      
end

module Client = Huxiang.Node_lwt.Make(Protocol)

let _ =
  Client.start
    ~ingoing:["tcp://127.0.0.1:5556"]
    ~outgoing:["tcp://127.0.0.1:5557"]

