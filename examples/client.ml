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

  let alive id nonce =
    O.Alive { client_id = id; nonce }

  let dead_state =
    { initial_state with status = Dead; nonce = -1 }

  (* Assuming the initial message to Mother will be 
     Hello { client_id = initial_state.id; nonce = initial_state.nonce }.
     It would be good if that initial message was part of the protocol.
  *)
  let transition state in_msg =
    let result =
    match state.status with
    | Dead   -> (dead_state, None)
    | Unborn ->
      (match in_msg with
       | I.Ack (O.Hello { client_id; nonce })
         when (client_id = initial_state.id && nonce = initial_state.nonce) ->
         let state = { state with
                       status = Alive;
                       nonce  = state.nonce + 1
                     } in
         (state, Some (alive state.id state.nonce))
       | _ ->
         (dead_state, None)
      )
    | Alive ->
      (match in_msg with
       | I.Ack (O.Alive { client_id; nonce })
         when client_id = state.id && nonce = state.nonce ->
         let state = { state with
                       nonce  = state.nonce + 1
                     } in
         (state, Some (alive state.id state.nonce))
       | _ ->
         (dead_state, None)
      )
    in
    Lwt.return result
      
end

module Client = Huxiang.Client.Make(Protocol)

let _ =
  Client.start
    ~port:33333
    ~log_callback:(fun s -> Printf.printf "client: %s\n%!" s)
    ~initial_message:(Protocol.O.Hello { client_id = "TODO parameterize protocol by id"; nonce = 0 })

