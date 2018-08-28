open Batteries
open Huxiang
open Huxiang.Types

module Ping =
struct

  module I = Messages.PongMsg
  module O = Messages.PingMsg

  type input  = I.t
  type output = O.t Address.multi_dest
               
  type state = { counter : int }
  [@@deriving show]

  let name = Name.atom "ping"

  let rec main_loop { counter } =
    Process.with_input (fun (I.Pong i) ->
        if i = counter then
          let state  = { counter = counter + 1 } in
          let output = Address.(O.Ping state.counter @. Directory.pong_node) in
          Process.continue_with ~output state main_loop
        else
          (Lwt_io.print "ping: dead state reached";%lwt
           Process.stop { counter })
      )

  let process state =
    let output = Address.(O.Ping 0 @. Directory.pong_node) in
    Process.without_input
      (Process.continue_with ~output state main_loop)

  let thread =
    {
      Process.move  = process;
      Process.state = { counter = 0 }
    }
              
end

let compiled_ping :> (module NetProcess.S) = NetProcess.compile (fun _ -> Ping.I.bin_reader_t) Ping.O.bin_writer_t (module Ping)

module EvilPing =
struct

  module I = Messages.PongMsg
  module O = Messages.PingMsg

  type input  = I.t
  type output = O.t Address.multi_dest
               
  type state = unit
  [@@deriving show]

  let name = Name.atom "ping"

  let rec main_loop () =
    Process.with_input (fun (I.Pong i) ->
        let output = Address.(O.Ping (Random.int 42) @. Directory.pong_node) in
        Process.continue_with ~output () main_loop
      )

  let process state =
    let output = Address.(O.Ping 0 @. Directory.pong_node) in
    Process.without_input
      (Process.continue_with ~output state main_loop)

  let thread =
    {
      Process.move  = process;
      Process.state = ()
    }
              
end

let compiled_evil_ping :> (module NetProcess.S) = NetProcess.compile (fun _ -> EvilPing.I.bin_reader_t) EvilPing.O.bin_writer_t (module EvilPing)

module Pong =
struct

  module I = Messages.PingMsg
  module O = Messages.PongMsg

  type input  = I.t
  type output = O.t Address.multi_dest

               
  type state = unit
  [@@deriving show]

  let name = Name.atom "pong"

  let rec main_loop state =
    Process.with_input (fun (I.Ping i) ->
        let output = Address.(O.Pong i @. Directory.ping_node) in
        Process.continue_with ~output state main_loop
      )

  let thread =
    {
      Process.move  = main_loop;
      Process.state = ()
    }
      
end

let compiled_pong :> (module NetProcess.S) = NetProcess.compile (fun _ -> Pong.I.bin_reader_t) Pong.O.bin_writer_t (module Pong)

let processes =
  [ compiled_ping;
    compiled_pong ]

let addresses =
  [ Directory.ping_node;
    Directory.pong_node
  ]

module PingPongForPing =
  Coalesce.Make
    (struct
      let processes = processes
      let addresses = addresses
      let owner     = Directory.ping_node.Address.owner
    end)
    (struct
      let scheduler = Process.uniform_random_scheduler
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
