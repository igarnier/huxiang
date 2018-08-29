open Types
open Bin_prot.Std

type notification_kind =
  | Transition of { t_index : int }
  | NoTransition
[@@deriving eq, bin_io]

type notification = 
  { nkind  : notification_kind;
    inputs : NetProcess.Input.t list }
[@@deriving eq, bin_io]

type ('l, 'i) input =
  (* | Leader of { proof : 'l } *)
  | INotification of { proof : 'l; notif : notification }
  | IInput of 'i
[@@deriving bin_io]

type 'l output_data =
  | ONotification of { proof : 'l; notif : notification }
  | OOutput of Types.HuxiangBytes.t
[@@deriving bin_io]

module Replica(P : NetProcess.S)(S : Process.Scheduler)(L : Leadership.S)(C : Address.Clique) :
  Process.S with type input = (L.t, P.input) input
             and type output = L.t output_data Address.multi_dest
=
struct

  type nonrec input = (L.t, P.input) input

  type output = L.t output_data Address.multi_dest

  module Data =
  struct

    type t = notification
    [@@deriving eq]

    let consistent = equal

  end

  module Chain = Chain.Make(Data)(L)

  type buffer = P.input Batteries.Deque.t

  type state =
    {
      (* proc    : (module NetProcess.S); *)
      proc    : P.state NetProcess.t;
      fbuff   : buffer; (* future buffer *)
      pbuff   : buffer; (* present buffer *)
      chain   : Chain.t;
      current : Crypto.Hash.t
    }

  let show_state _ = "opaque"

  let broadcast (msg : L.t output_data) =
    let open Address in
    {
      dests = C.addresses;
      msg
    }

  let rewrap_output (outp : P.output option) : output option =
    match outp with
    | None -> None
    | Some outp ->
      Some { outp 
             with msg = OOutput outp.msg
           }

  let name = Name.inter P.name

  let put_message_in_future msg state =
    { state with fbuff = Batteries.Deque.snoc state.fbuff msg }

  let put_message_in_present msg state =
    { state with pbuff = Batteries.Deque.snoc state.pbuff msg }

  let validate_leadership state proof =
    { state with chain = Chain.insert_proof proof state.chain }

  let play_transition state transition =
    match transition with
    | Process.NoInput code ->
      let%lwt { Process.output; next } = code in
      let state  = { state with proc = next } in
      let output = rewrap_output output in
      Lwt.return (Some (state, output))
    | Process.Input f ->
      (match Batteries.Deque.front state.pbuff with
       | None -> 
         Lwt.return None
       | Some (inp, pbuff) ->
         let%lwt { output; next } = f inp in
         let state  = { state with pbuff; proc = next } in
         let output = rewrap_output output in
         Lwt.return (Some (state, output))
      )

  let do_transition state index =
    let ks  = Process.evolve state.proc in
    let len = List.length ks in
    if index < 0 || index >= len then
      Lwt.return None
    else
      let transition = List.nth ks index in
      match%lwt play_transition state transition with
      | None -> 
        Lwt.return None
      | Some (state, output) ->
        Lwt.return (Some (state, output))

  let try_to_transition state =
    let ks = Process.evolve state.proc in
    (* 1. extract the playable transitions *)
    (* is input buffer empty? *)
    let buffer_empty = Batteries.Deque.is_empty state.pbuff in
    (* a transition is playable if it is NoInput or 
       (Input & not buffer_empty). *)
    let _, playable =
      List.fold_left (fun (i, acc) x ->
          match x with
          | Process.NoInput _ -> (i + 1, (i, x) :: acc)
          | Process.Input _   ->
            if buffer_empty then
              (i + 1, acc)
            else
              (i + 1, (i,x) :: acc)
        ) (0, []) ks
    in
    (* 2. apply scheduler to pick a transition *)
    match S.scheduler (List.map snd playable) with
    | None   -> Lwt.return None
    | Some i ->
      (let index, transition = List.nth playable i in
       match%lwt play_transition state transition with
       | None -> 
         Lwt.return None
       | Some (state, output) ->
         Lwt.return (Some (index, state, output))
      )

  let blame proof msg =
    let leader_pkey = Crypto.Public.show (L.leader proof) in
    Lwt.fail_with @@ msg^" Originator: "^leader_pkey

  let rec process state =
    Process.without_input begin
      let head = Chain.get_node state.chain state.chain.Chain.head in
      match L.extend head.Chain.proof with
      | None ->
        Process.continue_with state wait_for_input
      | Some proof ->
        let state = validate_leadership state proof in
        let node  = Chain.get_node state.chain state.current in
        Process.continue_with state (synch node.next)
    end

  and wait_for_input state =
    Process.with_input begin function
      (* | Leader { proof } ->        
       *   let state = validate_leadership state proof in
       *   let node  = Chain.get_node state.chain state.current in
       *   Process.continue_with state (synch node.next) *)
      | INotification { proof; notif } ->
        let state = validate_leadership state proof in
        let hash  = L.hash proof in
        let res   = Chain.add_to_existing_node notif hash state.chain in
        let%lwt chain =
          match res with
          | Chain.Consistent chain -> Lwt.return chain
          | Chain.Inconsistent(chain, d1, d2, proof) ->
            blame proof "Inconsistency detected."
        in
        let state = { state with chain } in
        let node  = Chain.get_node state.chain state.current in
        Process.continue_with state (synch node.next)
      | IInput i ->
        let state = put_message_in_future i state in
        Process.continue_with state process
    end

  and synch node state =
    Process.without_input begin
      match node with
      | None ->
        Process.continue_with state process
      | Some node ->
        let state = { state with current = node.Chain.hash } in
        (match node.data with
         | { nkind = NoTransition; inputs } :: _ ->
           let pbuff = Batteries.Deque.append_list state.pbuff inputs in
           let state = { state with pbuff } in
           Process.continue_with state (synch node.next)
         | { nkind = Transition { t_index }; inputs } :: _ ->
           (match%lwt do_transition state t_index with
            | None ->
              (* Inconsistency! *)
              let message = 
                "Inconsistency detected. Could not perform transition." 
              in
              blame node.proof message
            | Some (state, output) ->
              let pbuff = Batteries.Deque.append_list state.pbuff inputs in
              let state = { state with pbuff } in
              Process.continue_with ?output state (synch node.next))
         | [] ->
           (* The only way the data is empty is if we're leader. *)
           (match%lwt try_to_transition state with
            | None ->
              notify_no_transition state node
            | Some result ->
              notify_transition result node
           )
        )
    end

  and notify_no_transition state node =
    let notif  = { nkind  = NoTransition;
                   inputs = Batteries.Deque.to_list state.fbuff } in
    let hash   = L.hash node.Chain.proof in
    let result =
      Chain.add_to_existing_node notif hash state.chain
    in
    let%lwt chain =
      match result with
      | Chain.Consistent chain -> Lwt.return chain
      | Chain.Inconsistent _ ->
        Lwt.fail_with @@
        "huxiang/replica: impossible case. Bug found, please report."
    in
    let state  = 
      let pbuff = Batteries.Deque.append state.pbuff state.fbuff in
      let fbuff = Batteries.Deque.empty in
      { state with chain; pbuff; fbuff }
    in
    let output = broadcast (ONotification { proof = node.proof; notif }) in
    Process.continue_with ~output state (synch node.next)

  and notify_transition (t_index, state, output) node =
    Process.continue_with ?output state
      (fun state -> Process.without_input begin
           let notif  = { nkind  = Transition { t_index };
                          inputs = Batteries.Deque.to_list state.fbuff
                        }
           in
           let hash   = L.hash node.proof in
           let result =
             Chain.add_to_existing_node notif hash state.chain
           in
           let%lwt chain =
             match result with
             | Chain.Consistent chain -> Lwt.return chain
             | Chain.Inconsistent _ ->
               Lwt.fail_with @@
               "huxiang/replica: impossible case. "^
               "Bug found, please report."
           in
           let state  = 
             let pbuff = Batteries.Deque.append state.pbuff state.fbuff in
             let fbuff = Batteries.Deque.empty in
             { state with chain; pbuff; fbuff }
           in
           let output = broadcast (ONotification { proof = node.proof; notif }) in
           Process.continue_with ~output state (synch node.next)
         end
      )

  let initial_state =
    let chain = Chain.create () in
    {
      proc  = P.thread;
      fbuff = Batteries.Deque.empty;
      pbuff = Batteries.Deque.empty;
      chain;
      current = chain.Chain.head
    }

  let thread =
    {
      Process.move = process;
             state = initial_state
    }
           
end

module Serializer(L : Leadership.S) :
  NetProcess.Serializer 
  with type t = L.t output_data
=
struct

  open Bin_prot
  
  type t = L.t output_data
  [@@deriving bin_io]

  let serializer = bin_writer_t
    
end

module Deserializer(L : Leadership.S)(C : Address.Clique) :
  NetProcess.Deserializer 
  with type t = (L.t, NetProcess.Input.t) input
=
struct

  open Bin_prot
  
  type t = (L.t, NetProcess.Input.t) input
  [@@deriving bin_io]

  let deserializer pubkey =
    match pubkey with
    | None ->
      failwith @@ "huxiang/replica/deserializer/deserialize: "^
                  "input message is not signed"
    | Some key ->
      let originator_in_clique =
        List.exists (fun { Address.owner } ->
            Crypto.Public.equal owner key
          ) C.addresses
      in
      if originator_in_clique then
        bin_reader_t
      else
        let read : t Bin_prot.Read.reader = fun buf ~pos_ref ->
          IInput (NetProcess.Input.bin_read_t buf ~pos_ref)
        in
        let vtag_read = fun buf ~pos_ref tag ->
          IInput (NetProcess.Input.bin_reader_t.vtag_read buf ~pos_ref tag)
        in
        {
          Bin_prot.Type_class.read; 
          vtag_read
        }

end
 

module Make(P : NetProcess.S)(S : Process.Scheduler)(L : Leadership.S)(C : Address.Clique) =
struct
  module Dsz = Deserializer(L)(C)

  module Sz  = Serializer(L)

  module Rep = Replica(P)(S)(L)(C)

  let result = NetProcess.compile Dsz.deserializer Sz.serializer (module Rep)

  module Outcome = (val result)

  include Outcome
end
