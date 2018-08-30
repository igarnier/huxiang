open Batteries
open Huxiang

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
          Lwt_log.debug_f "process Ping emitting %d\n" state.counter;%lwt
          Process.continue_with ~output state main_loop
        else
          (Lwt_io.print "ping: dead state reached";%lwt
           Process.stop { counter })
      )

  let process state =
    let output = Address.(O.Ping 0 @. Directory.pong_node) in
    Process.without_input
      begin
        Lwt_log.debug_f "process Ping emitting inital output\n";%lwt
        Process.continue_with ~output state main_loop
      end

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
    Process.with_input (fun (I.Pong _) ->
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
        Lwt_log.debug_f "process Pong replying %d\n" i;%lwt
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


module EvilPingParams : Product.Params =
struct
  let addresses = Directory.[ping_node; pong_node]
  let processes = [compiled_evil_ping; compiled_pong]
  let owner     = Directory.pong_pkey
end

module PingParams : Product.Params =
struct
  let addresses = Directory.[ping_node; pong_node]
  let processes = [compiled_ping; compiled_pong]
  let owner     = Directory.pong_pkey
end

module PongParams : Product.Params =
struct
  include PingParams
  let owner     = Directory.pong_pkey
end

module Scheduler : Process.Scheduler =
struct
  let scheduler = Process.uniform_random_scheduler
end

module Clique : Address.Clique =
struct
  include PingParams
end
  

module LeadershipForPing =
  Leadership.RoundRobin(Clique)(Directory.PingCredentials)

module LeadershipForPong =
  Leadership.RoundRobin(Clique)(Directory.PongCredentials)

module PingPongForPing =
  Coalesce.Make
    (PingParams)
    (Scheduler)
    (LeadershipForPing)

module EvilPingPongForPing =
  Coalesce.Make
    (PingParams)
    (Scheduler)
    (LeadershipForPing)

module PingPongForPong =
  Coalesce.Make
    (PongParams)
    (Scheduler)
    (LeadershipForPong)
