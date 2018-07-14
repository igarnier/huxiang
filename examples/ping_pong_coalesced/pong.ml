open Batteries
open Huxiang.Types

module PongProcess : Process =
struct

  module I = Messages.PingMsg
  module O = Messages.PongMsg
               
  type state = unit

  let initial_state   = ()

  let show_state _ = "()"

  let rec process =
    Input (fun state (I.Ping i) ->
        Lwt.return (state, Some (O.Pong i), process)
      )
      
end
