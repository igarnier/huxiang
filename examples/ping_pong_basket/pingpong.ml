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
          let output = Address.(O.Ping (counter+1) @+ Directory.[pong_node; basket_node]) in
          Process.continue_with ~output state main_loop
        else
          Process.stop { counter }
      )

  let process state =
    let output = Address.(O.Ping 0 @+ Directory.[pong_node; basket_node]) in
    Process.without_input
      (Process.continue_with ~output state main_loop)


  let thread =
    {
      Process.state = { counter = 0 };
      move = process
    }

end

let compiled_ping :> (module NetProcess.S) = NetProcess.compile (fun _ -> Ping.I.bin_reader_t) Ping.O.bin_writer_t (module Ping)


module Pong =
struct

  module O = Messages.PongMsg
  module I = Messages.PingMsg

  type input  = I.t
  type output = O.t Address.multi_dest
               
  type state = unit
  [@@deriving show]

  let name = Name.atom "pong"

  let rec main_loop state =
    Process.with_input (fun (I.Ping i) ->
        let output = Address.(O.Pong i @+ Directory.[ping_node; basket_node]) in
        Process.continue_with ~output state main_loop
      )

  let thread =
    {
      Process.move  = main_loop;
      Process.state = ()
    }
      
end

let compiled_pong :> (module NetProcess.S) = NetProcess.compile (fun _ -> Pong.I.bin_reader_t) Pong.O.bin_writer_t (module Pong)


(* ------------------------------------------------------------------------------------------------------ *)

let processes =
  [ compiled_ping;
    compiled_pong ]

let addresses =
  [ Directory.ping_node;
    Directory.pong_node
  ]

module PingParams : Product.Params =
struct
  let addresses = Directory.[ping_node; pong_node]
  let processes = [compiled_ping; compiled_pong]
  let owner     = Directory.ping_pkey
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
    (Directory.PingCredentials)

module PingPongForPong =
  Coalesce.Make
    (PongParams)
    (Scheduler)
    (LeadershipForPong)
    (Directory.PongCredentials)
