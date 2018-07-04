open Batteries
open Huxiang.Types

module Protocol : Protocol_Sig =
struct

  module I = Messages.ClientToServer
  module O = Messages.ServerToClient

  type input = {
    msg    : I.t;
    tstamp : float
  }
  
  type state = (I.client_id * (float * int)) list

  let initial_state = []

  let transition state in_msg =
    let result =
      match in_msg with
      | I.Hello { client_id; nonce } ->
        let state =
          (match List.assoc_opt client_id state with
           | None ->
             let t = Unix.gettimeofday () in
             (client_id, (t, nonce)) :: state
           | Some _ ->
             failwith "transition: client already Hello'd")
        in
        (state, Some (O.Ack in_msg))
      | I.Alive { client_id; nonce } ->
        (match List.assoc_opt client_id state with
         | None ->
           failwith "transition: client didn't Hello"
         | Some (t', nonce') ->
           let t = Unix.gettimeofday () in
           if nonce' >= nonce || (t -. t' >= 30.0) then
             (state, (Some O.Die))
           else
             (state, Some (O.Ack in_msg))
        )
    in
    Lwt.return result
      
end

module Server = Huxiang.Server.Make(Protocol)

let _ =
  Server.start
    ~port:(int_of_string Sys.argv.(1))
    ~log_callback:(fun s -> Printf.printf "serv: %s\n%!" s)
