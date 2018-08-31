open Huxiang

module Client =
struct

  module I = Messages.Nothing
  module O = Messages.ClientToBroker

  type input  = I.t
  type output = O.t Address.multi_dest

  type state = { money : int } [@@deriving show]

  let name = Name.atom "client"

  let rec main_loop state =
    Process.without_input
      (Lwt_unix.sleep 1.0;%lwt
       Lwt_io.printf "money left: %d\n" state.money;%lwt
       if state.money > 0 then
         let state  = { money = state.money - 1 } in
         let output = Address.(O.Payement 1 @. Directory.broker_node) in
         Process.continue_with ~output state main_loop
       else
         Process.stop state
      )

  let thread =
    {
      Process.move = main_loop;
      state = { money = 1003 }
    }

end

let compiled_client :> (module NetProcess.S) = 
  NetProcess.compile (fun _ -> Client.I.bin_reader_t) Client.O.bin_writer_t (module Client)

module Service =
struct

  module I = Messages.BrokerToService
  module O = Messages.Nothing

  type input  = I.t
  type output = O.t Address.multi_dest

  type state = { money : int } [@@deriving show]

  let name = Name.atom "service"

  let service_msg bulk state =
    Lwt_io.printf "service: got bulk payement: %d, total = %d\n" 
      bulk state.money

  let rec main_loop state =
    Process.with_input (fun (I.BulkPayement i) ->
        let state = { money = state.money + i } in
        service_msg i state;%lwt
        Process.continue_with state main_loop
      )

  let thread =
    {
      Process.move = main_loop; state = { money = 0 }
    }

end

let compiled_service :> (module NetProcess.S) = 
  NetProcess.compile (fun _ -> Service.I.bin_reader_t) Service.O.bin_writer_t (module Service)

module Broker =
struct

  module I = Messages.ClientToBroker
  module O = Messages.BrokerToService

  type input  = I.t
  type output = O.t Address.multi_dest

  type state = { personal : int; flow : int } [@@deriving show]

  let name = Name.atom "broker"

  let broker_msg payement state =
    Lwt_io.printf
      "broker: got payement: %d, flow = %d, personal = %d\n" 
      payement state.flow state.personal

  let rec main_loop state =
    Process.with_input (fun (I.Payement i) ->
        broker_msg i state;%lwt
        let state = { state with flow = state.flow + i } in
        if state.flow > 10 then
          let for_service = state.flow - 1 and for_me = 1 in
          let state       = { personal = state.personal + for_me;
                              flow     = 0 } in
          let output = 
            Address.(O.BulkPayement for_service @. Directory.service_node)
          in
          Process.continue_with ~output state main_loop
        else
          Process.continue_with state main_loop
      )

  let thread =
    {
      Process.move = main_loop; state = { personal = 0; flow = 0 }
    }

end

let compiled_broker :> (module NetProcess.S) = 
  NetProcess.compile (fun _ -> Broker.I.bin_reader_t) Broker.O.bin_writer_t (module Broker)

module BrokerParams : Product.Params =
struct
  let addresses = Directory.[service_node; broker_node]
  let processes = [compiled_service; compiled_broker]
  let owner     = Directory.BrokerCred.public_key
end

module ServiceParams : Product.Params =
struct
  include BrokerParams
  let owner     = Directory.ServiceCred.public_key
end

module Scheduler : Process.Scheduler =
struct
  let scheduler = Process.uniform_random_scheduler
end

module Clique : Address.Clique =
struct
  include BrokerParams
end
  
module LeadershipForBroker =
  Leadership.RoundRobin(Clique)(Directory.BrokerCred)

module LeadershipForService =
  Leadership.RoundRobin(Clique)(Directory.ServiceCred)

module SB_Serv = 
  Coalesce.Make
    (ServiceParams)
    (Scheduler)
    (LeadershipForService)
    (Directory.ServiceCred)

module SB_Brok = 
  Coalesce.Make
    (BrokerParams)
    (Scheduler)
    (LeadershipForBroker)
    (Directory.BrokerCred)
