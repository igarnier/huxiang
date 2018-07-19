open Batteries
open Huxiang
open Huxiang.Types

module Ping =
struct

  module I = Messages.PongMsg
  module O = Messages.PingMsg
               
  type state =
    | Alive of { counter : int }
    | Dead
  [@@deriving show]

  let rec main_loop state =
    Process.with_input (fun (I.Pong i) ->
        match state with
        | Alive { counter } when i = counter ->
          let state = Alive { counter = counter + 1 } in
          Process.continue_with ~output:(O.Ping (counter + 1)) state main_loop
        | _ ->
          let state = Dead in
          Process.continue_with state main_loop
      )

  let process state =
    Process.without_input
      (Process.continue_with ~output:(O.Ping 0) state main_loop)

  let thread =
    {
      Process.move  = process;
      Process.state = Alive { counter = 0 }
    }
              
end

module Pong =
struct

  module I = Messages.PingMsg
  module O = Messages.PongMsg
               
  type state = unit
  [@@deriving show]

  let rec main_loop state =
    Process.with_input (fun (I.Ping i) ->
        Process.continue_with ~output:(O.Pong i) state main_loop
      )

  let thread =
    {
      Process.move  = main_loop;
      Process.state = ()
    }
      
end
