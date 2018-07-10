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
  
  type state = (I.msg * (float * int)) list

  let initial_state = []
                      
  let initial_message = None

  let transition state in_msg =
    match in_msg with
    | I.Ping msg ->
      failwith ""
        
    
end

module Server = Huxiang.Node_lwt.Make(Protocol)

let _ =
  Server.start
    ~ingoing:["tcp://127.0.0.1:5557"]
    ~outgoing:["tcp://127.0.0.1:5556"]
