open Batteries
open Huxiang.Types

module Protocol : ProtocolLwt_Sig =
struct

  module I = Messages.ClientToServer
  module O = Messages.ServerToClient

  type input = {
    msg    : I.t;
    tstamp : float
  }
  
  type state = (I.client_id * (float * int)) list

  let initial_state = []

  let initial_message =
    O.ServerStartup

  let transition state in_msg =
    match in_msg with
    | I.Hello { client_id; nonce } ->
      (match List.assoc_opt client_id state with
       | None ->
         let t = Unix.gettimeofday () in
         let state = (client_id, (t, nonce)) :: state in
         Lwt.return (state, Some (O.Ack in_msg))
       | Some _ ->
         Lwt.fail_with "transition: client already Hello'd")
    | I.Alive { client_id; nonce } ->
      (match List.assoc_opt client_id state with
       | None ->
         Lwt.fail_with "transition: client didn't Hello"
       | Some (t', nonce') ->
         let t = Unix.gettimeofday () in
         if nonce' >= nonce || (t -. t' >= 30.0) then
           (Lwt_io.print "transition: server died\n";%lwt
            Lwt.return (state, (Some O.Die)))
         else
           Lwt.return (state, Some (O.Ack in_msg))
      )

end

module Server = Huxiang.Node_lwt.Make(Protocol)

let _ =
  Server.start
    ~ingoing:["tcp://127.0.0.1:5557"]
    ~outgoing:["tcp://127.0.0.1:5556"]
