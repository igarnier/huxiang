open Huxiang
open Types

module Client =
struct

  module I = Messages.Nothing
  module O = Messages.ClientToBroker

  type state = { money : int } [@@deriving show]

  let initial_state = { money = 1003 }

  let rec process =
    NoInput (fun state ->
        if state.money > 0 then
          let state = { money = state.money - 1 } in
          Lwt.return (state, Some (O.Payement 1), process)
        else
          Lwt.return (state, None, process)
      )

end

module Service =
struct

  module I = Messages.BrokerToService
  module O = Messages.Nothing

  type state = { money : int } [@@deriving show]

  let initial_state = { money = 0 }

  let rec process =
    Input (fun state (I.BulkPayement i) ->
        Lwt.return ({ money = state.money + i}, None, process)
      )

end


module Broker =
struct

  module I = Messages.ClientToBroker
  module O = Messages.BrokerToService

  type state = { personal : int; flow : int } [@@deriving show]

  let initial_state = { personal = 0; flow = 0 }

  let rec process =
    Input (fun state (I.Payement i) ->
        let state = { state with flow = state.flow + i } in
        if state.flow > 100 then
          let for_service = state.flow - 1 in
          let for_me      = 1 in
          let state       = { personal = state.personal + for_me;
                              flow     = 0 }
          in
          Lwt.return (state, Some (O.BulkPayement for_service), process)
        else
          Lwt.return (state, None, process)
      )

end

