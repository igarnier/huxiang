open Batteries
open Huxiang
open Huxiang.Types

module Ping : Process.S =
struct

  module I = Messages.PongMsg
  module O = Messages.PingMsg
               
  type state = { counter : int }
  [@@deriving show]

  let name = Process.Name.atom "ping"

  let rec main_loop { counter } =
    Process.with_input (fun (I.Pong i) ->
        if i = counter then
          let state  = { counter = counter + 1 } in
          let output =
            { Process.Address.msg = O.Ping (counter + 1); 
              dests = [ (Directory.pong_node, Root) ] }
          in
          Process.continue_with ~output state main_loop
        else
          (Lwt_io.print "ping: dead state reached";%lwt
           Process.stop { counter })
      )

  let process state =
    let output =
      { Process.Address.msg = O.Ping 0; 
        dests = [ (Directory.pong_node, Root) ] }
    in
    Process.without_input
      (Process.continue_with ~output state main_loop)

  let thread =
    {
      Process.move  = process;
      Process.state = { counter = 0 }
    }
              
end

module EvilPing : Process.S =
struct

  module I = Messages.PongMsg
  module O = Messages.PingMsg
               
  type state = unit
  [@@deriving show]

  let name = Process.Name.atom "ping"

  let rec main_loop () =
    Process.with_input (fun (I.Pong i) ->
        let output =
          { Process.Address.msg = O.Ping (Random.int 42); 
            dests = [ (Directory.pong_node, Root) ] }
        in
        Process.continue_with ~output () main_loop
      )

  let process state =
    let output =
      { Process.Address.msg = O.Ping 0; 
        dests = [ (Directory.pong_node, Root) ] }
    in
    Process.without_input
      (Process.continue_with ~output state main_loop)

  let thread =
    {
      Process.move  = process;
      Process.state = ()
    }
              
end

module Pong : Process.S =
struct

  module I = Messages.PingMsg
  module O = Messages.PongMsg
               
  type state = unit
  [@@deriving show]

  let name = Process.Name.atom "pong"

  let rec main_loop state =
    Process.with_input (fun (I.Ping i) ->
        let output =
          { Process.Address.msg  = O.Pong i; 
            dests = [ (Directory.ping_node, Root) ] }
        in
        Process.continue_with ~output state main_loop
      )

  let thread =
    {
      Process.move  = main_loop;
      Process.state = ()
    }
      
end

module PingPongForPing = 
  Coalesce.Prod
    (Ping)
    (Pong)
    (TrivialLeader)
    (struct 
      let left_id  = Directory.ping_node.owner
      let right_id = Directory.pong_node.owner
      let owner    = left_id
    end)

module EvilPingPongForPing = 
  Coalesce.Prod
    (EvilPing)
    (Pong)
    (TrivialLeader)
    (struct 
      let left_id  = Directory.ping_node.owner
      let right_id = Directory.pong_node.owner
      let owner    = left_id
    end)

module PingPongForPong = 
  Coalesce.Prod
    (Ping)
    (Pong)
    (TrivialLeader)
    (struct 
      let left_id  = Directory.ping_node.owner
      let right_id = Directory.pong_node.owner
      let owner    = right_id
    end)
