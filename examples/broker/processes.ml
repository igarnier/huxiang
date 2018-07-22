open Huxiang
open Types

module Client : Process.S =
struct

  module I = Messages.Nothing
  module O = Messages.ClientToBroker

  type state = { money : int } [@@deriving show]

  let name = Process.Name.atom "client"

  let rec main_loop state =
    Process.without_input
      (Lwt_unix.sleep 1.0;%lwt
       Lwt_io.printf "money left: %d\n" state.money;%lwt
       if state.money > 0 then
         let state = { money = state.money - 1 } in
         let output = {
           Process.Address.msg = O.Payement 1;
           dests = [ (Directory.broker_node, Root) ]
         } in
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

module ClientAfterProduct : Process.S =
struct

  module I = Messages.Nothing
  module O = Messages.ClientToBroker

  type state = { money : int } [@@deriving show]

  let name = Process.Name.atom "client"

  let rec main_loop state =
    Process.without_input
      (Lwt_unix.sleep 1.0;%lwt
       Lwt_io.printf "client: money left: %d\n" state.money;%lwt
       if state.money > 0 then
         let state = { money = state.money - 1 } in
         let output = {
           Process.Address.msg = O.Payement 1;
           dests = [ (Directory.broker_node, Access(Directory.broker_node.pname, Root));
                     (Directory.service_node, Access(Directory.broker_node.pname, Root));
                   ]
         } in
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


module Service : Process.S =
struct

  module I = Messages.BrokerToService
  module O = Messages.Nothing

  type state = { money : int } [@@deriving show]

  let name = Process.Name.atom "service"

  let rec main_loop state =
    Process.with_input (fun (I.BulkPayement i) ->
        let state = { money = state.money + i } in
        Lwt_io.printf "service: got bulk payement: %d, total = %d\n" i state.money;%lwt
        Process.continue_with state main_loop
      )

  let thread =
    {
      Process.move = main_loop; state = { money = 0 }
    }

end


module Broker : Process.S =
struct

  module I = Messages.ClientToBroker
  module O = Messages.BrokerToService

  type state = { personal : int; flow : int } [@@deriving show]

  let name = Process.Name.atom "broker"

  let rec main_loop state =
    Process.with_input (fun (I.Payement i) ->
        Lwt_io.printf "broker: got payement: %d, flow = %d, personal = %d\n" i state.flow state.personal;%lwt
        let state = { state with flow = state.flow + i } in
        if state.flow > 10 then
          let for_service = state.flow - 1 in
          let for_me      = 1 in
          let state       = { personal = state.personal + for_me;
                              flow     = 0 }
          in
          let output = {
            Process.Address.msg = O.BulkPayement for_service;
            dests = [ (Directory.service_node, Root) ]
          } in
          Process.continue_with ~output state main_loop
        else
          Process.continue_with state main_loop
      )

  let thread =
    {
      Process.move = main_loop; state = { personal = 0; flow = 0 }
    }

end

module SB_Serv = 
  Coalesce.Prod
    (Service)
    (Broker)
    (TrivialLeader)
    (struct 
      let left_id  = Directory.service_node.owner
      let right_id = Directory.broker_node.owner
      let owner    = left_id
    end)

module SB_Brok = 
  Coalesce.Prod
    (Service)
    (Broker)
    (TrivialLeader)
    (struct 
      let left_id  = Directory.service_node.owner
      let right_id = Directory.broker_node.owner
      let owner    = right_id
    end)
