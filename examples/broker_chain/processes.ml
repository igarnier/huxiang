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
         let state  = { money = state.money - 1 } in
         let output = Process.(O.Payement 1 @ Directory.broker_node) in
         Process.continue_with ~output state main_loop
       else
         Process.stop state
      )

  let thread =
    {
      Process.move = main_loop;
      state = { money = 1000 }
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
         let state  = { money = state.money - 1 } in
         let output = Process.(
             O.Payement 1 @+
             [ (Directory.broker_node_product, 
                Directory.broker_node.pname --> Root);
               (Directory.service_node_product, 
                Directory.broker_node.pname --> Root) ]
           )
         in
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

module Service : Huxiang.Process.S =
struct
  module I =
  struct
    type t = Messages.broker_to_service
    [@@deriving eq, yojson, show]

    let serialize x =
      Yojson.Safe.to_string (to_yojson x)

    let deserialize s pth =    
      match pth with
      | Process.Address.Root ->
        (match of_yojson (Yojson.Safe.from_string s) with
         | Ok x -> x
         | Error s -> failwith s)
      | _ ->
        failwith "pingmsg/deserialize: wrong path"
  end

  module O =
  struct
    type t = Messages.service_to_mother
    [@@deriving eq, yojson, show]

    let serialize x =
      Yojson.Safe.to_string (to_yojson x)

    let deserialize s pth =    
      match pth with
      | Process.Address.Root ->
        (match of_yojson (Yojson.Safe.from_string s) with
         | Ok x -> x
         | Error s -> failwith s)
      | _ ->
        failwith "pingmsg/deserialize: wrong path"
  end

  type state = { money : int; last_keepalive : float } [@@deriving show]

  let name = Process.Name.atom "service"

  let service_msg bulk state =
    Lwt_io.printf "service: got bulk payement: %d, total = %d\n" 
      bulk state.money

  let rec deposit state =
    Process.without_input
      begin
        let output = Process.(`ServiceDeposit @ Directory.service_mother_node) in
        Process.continue_with ~output state main_loop
      end

  and main_loop state =
    Process.with_input (fun (`BulkPayement i) ->
        let state = { state with money = state.money + i } in
        service_msg i state;%lwt
        Process.continue_with state write_to_mother
      )

  and write_to_mother state =
    Process.without_input
      begin
        let output = Process.(`ServiceKeepAlive @ Directory.service_mother_node) in
        Process.continue_with ~output state main_loop
      end
  
        
  let thread =
    {
      Process.move = deposit; state = { money = 0; last_keepalive = Unix.gettimeofday () }
    }

end

module Broker : Huxiang.Process.S =
struct

  module I = Messages.ClientToBroker
  module O =
  struct
    type t = [Messages.broker_to_service | Messages.broker_to_mother]
    [@@deriving eq, yojson, show]

    let serialize x =
      Yojson.Safe.to_string (to_yojson x)

    let deserialize s pth =    
      match pth with
      | Process.Address.Root ->
        (match of_yojson (Yojson.Safe.from_string s) with
         | Ok x -> x
         | Error s -> failwith s)
      | _ ->
        failwith "pingmsg/deserialize: wrong path"
  end

  type state = { personal : int; flow : int; } 
  [@@deriving show]

  let name = Process.Name.atom "broker"

  let broker_msg payement state =
    Lwt_io.printf
      "broker: got payement: %d, flow = %d, personal = %d\n" 
      payement state.flow state.personal

  let rec deposit state =
    Process.without_input
      begin
        let output = Process.(`BrokerDeposit @ Directory.broker_mother_node) in
        Process.continue_with ~output state main_loop
      end

  and main_loop state =
    Process.with_input (fun (I.Payement i) ->
        broker_msg i state;%lwt
        let state = { state with flow = state.flow + i } in
        if state.flow > 10 then
          let for_service = state.flow - 1 in
          let for_me      = 1 in
          let state       = { personal = state.personal + for_me;
                              flow     = 0 }
          in
          let output = Process.(`BulkPayement for_service @ Directory.service_node) in
          Process.continue_with ~output state write_to_mother
        else
          Process.continue_with state main_loop
      )

  and write_to_mother state =
    Process.without_input
      begin
        let output = Process.(`BrokerKeepAlive @ Directory.broker_mother_node) in
        Process.continue_with ~output state main_loop
      end

  let thread =
    {
      Process.move = deposit; 
      state = { personal = 0; 
                flow = 0 }
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
