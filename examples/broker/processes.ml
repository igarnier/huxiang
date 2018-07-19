open Huxiang
open Types

module Client =
struct

  module I = Messages.Nothing
  module O = Messages.ClientToBroker

  type state = { money : int } [@@deriving show]

  let rec main_loop state =
    Process.without_input
      (if state.money > 0 then
          let state = { money = state.money - 1 } in
          Process.continue_with ~output:(O.Payement 1) state main_loop
        else
          Process.stop state
      )

  let thread =
    {
      Process.move = main_loop;
      state = { money = 1003 }
    }

end

module Service =
struct

  module I = Messages.BrokerToService
  module O = Messages.Nothing

  type state = { money : int } [@@deriving show]

  let initial_state = { money = 0 }

  let rec main_loop state =
    Process.with_input (fun (I.BulkPayement i) ->
        let state = { money = state.money + i } in
        Process.continue_with state main_loop
      )

  let thread =
    {
      Process.move = main_loop; state = { money = 0 }
    }

end


module Broker =
struct

  module I = Messages.ClientToBroker
  module O = Messages.BrokerToService

  type state = { personal : int; flow : int } [@@deriving show]

  let rec main_loop state =
    Process.with_input (fun (I.Payement i) ->
        let state = { state with flow = state.flow + i } in
        if state.flow > 100 then
          let for_service = state.flow - 1 in
          let for_me      = 1 in
          let state       = { personal = state.personal + for_me;
                              flow     = 0 }
          in
          let output      = O.BulkPayement for_service in
          Process.continue_with ~output state main_loop
        else
          Process.continue_with state main_loop
      )

  let thread =
    {
      Process.move = main_loop; state = { personal = 0; flow = 0 }
    }

end

