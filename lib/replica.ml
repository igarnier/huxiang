open Bin_prot.Std

type notification_kind =
  | Transition of { t_index : int }
  | NoTransition
[@@deriving show, eq, bin_io]

type notification = 
  { nkind  : notification_kind;
    inputs : NetProcess.Input.t list }
[@@deriving show, eq, bin_io]

type ('l, 'i) input =
  | INotification of { proof : 'l; notif : notification }
  | IInput of 'i
[@@deriving bin_io]

type 'l output_data =
  | ONotification of { proof : 'l; notif : notification }
  | OOutput of Types.HuxiangBytes.t
[@@deriving bin_io]

module Replica(P : NetProcess.S)(S : Process.Scheduler)(L : Leadership.S)(C : Address.PointedClique) :
  Process.S with type input = (L.t, P.input) input
             and type output = L.t output_data Address.multi_dest
=
struct

  type nonrec input = (L.t, P.input) input

  type output = L.t output_data Address.multi_dest

  module Data =
  struct

    type t = notification
    [@@deriving eq, show]

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

  let lwt_debug fname msg =
    Lwt_log.debug_f "%s: %s" fname msg

  let lwt_info fname msg =
    Lwt_log.info_f "%s: %s" fname msg

  let show_state _ = "opaque"

  let broadcast =
    let open Address in
    let everyone_except_me =
      List.filter (fun { Address.owner; _ } ->
          not (Crypto.Public.equal owner C.owner)
        ) C.addresses
    in
    fun (msg : L.t output_data) ->
    {
      dests = everyone_except_me;
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

  (* let put_message_in_present msg state =
   *   { state with pbuff = Batteries.Deque.snoc state.pbuff msg } *)

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
    match Batteries.List.nth_opt ks index with
    | None            -> Lwt.return None
    | Some transition ->
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
      (match Batteries.List.nth_opt playable i with
       | None ->
         let playable_num = List.length playable in
         let msg = 
           Printf.sprintf "scheduler returned transition %d/%d" i playable_num
         in
         Lwt.fail_with @@ "huxiang/replica/try_to_transition:" ^ msg
       | Some (index, transition) ->
         (match%lwt play_transition state transition with
          | None -> 
            Lwt.return None
          | Some (state, output) ->
            Lwt.return (Some (index, state, output))
         )
      )

  let blame proof msg =
    let leader_pkey = Crypto.Public.show (L.leader proof) in
    Lwt.fail_with @@ msg^" Originator: "^leader_pkey
                     
  let rec process state =
    Process.without_input begin
      lwt_debug "huxiang/replica/process" "checking whether we are leader";%lwt
      let head = Chain.get_node state.chain state.chain.Chain.head in
      (* Are we leader? *)
      match L.extend head.Chain.proof with
      | None ->
        lwt_debug "huxiang/replica/process" "we are not leader";%lwt
        Process.continue_with state wait_for_input
      | Some proof ->
        lwt_debug "huxiang/replica/process" "we are leader";%lwt
        let state = validate_leadership state proof in
        let node  = Chain.get_node state.chain state.current in
        Process.continue_with state (synch node.next)
    end

  and wait_for_input state =
    Process.with_input begin
      function
      | INotification { proof; notif } ->
        lwt_info "huxiang/replica/wait_for_input" "received notification";%lwt
        let notif_s = show_notification notif in
        lwt_info "huxiang/replica/wait_for_input" notif_s;%lwt
        let state = validate_leadership state proof in
        let hash  = L.hash proof in
        let res   = Chain.add_to_existing_node notif hash state.chain in
        let%lwt chain =
          match res with
          | Chain.Consistent chain -> Lwt.return chain
          | Chain.Inconsistent(_, _, _, proof) ->
            blame proof "Inconsistency detected."
        in
        let state = { state with chain } in
        let node  = Chain.get_node state.chain state.current in
        Process.continue_with state (synch node.next)
      | IInput i ->
        lwt_debug "huxiang/replica/wait_for_input" "received external input";%lwt
        let state = put_message_in_future i state in
        Process.continue_with state process
    end

  and synch node state =
    Process.without_input begin
      lwt_debug "huxiang/replica/synch" "syncing local state with chain";%lwt
      match node with
      | None ->
        lwt_debug "huxiang/replica/synch" "syncing terminated";%lwt
        Process.continue_with state process
      | Some node_addr ->
        let node  = Chain.get_node state.chain node_addr in
        let state = { state with current = node.Chain.hash } in
        (match node.data with
         | { nkind = NoTransition; inputs } :: _ ->
           let msg = 
             Printf.sprintf "syncing NoTransition(%d)" (List.length inputs)
           in
           lwt_debug "huxiang/replica/synch" msg;%lwt
           let pbuff = Batteries.Deque.append_list state.pbuff inputs in
           let state = { state with pbuff } in
           Process.continue_with state (synch node.next)
         | { nkind = Transition { t_index }; inputs } :: _ ->
           let msg = 
             Printf.sprintf "syncing Transition(%d)" (List.length inputs)
           in
           lwt_debug "huxiang/replica/synch" msg;%lwt
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
           lwt_debug "huxiang/replica/synch" "trying to transition";%lwt
           (* The only way the data is empty is if we're leader. *)
           (match%lwt try_to_transition state with
            | None ->
              lwt_debug "huxiang/replica/synch" "no transition to play, notify";%lwt
              notify_no_transition state node
            | Some result ->
              lwt_debug "huxiang/replica/synch" "transition played, notify";%lwt
              notify_transition result node
           )
        )
    end

  and notify_no_transition state node =
    lwt_debug
      "huxiang/replica/notify_no_transition" 
      "notifying that no transition has been played";%lwt
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
    lwt_debug
      "huxiang/replica/notify_transition" 
      "notifying that some transition has been played";%lwt
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
           let output = 
             broadcast (ONotification { proof = node.proof; notif }) 
           in
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


module Make(P : NetProcess.S)(S : Process.Scheduler)(L : Leadership.S)(C : Address.Clique)(Cred : Crypto.Credentials) =
struct
  (* module Dsz = Deserializer(L)(C)
   * 
   * module Sz  = Serializer(L) *)

  module PClique : Address.PointedClique =
  struct
    include C
    let owner = Cred.public_key
  end


  module Rep = Replica(P)(S)(L)(PClique)

  module I =
  struct
    type t = (L.t, NetProcess.Input.t) input
    [@@deriving bin_io]
  end

  module O =
  struct
    type t = L.t output_data
    [@@deriving bin_io]
  end

  type input  = NetProcess.input
  type output = NetProcess.output

  type state = Rep.state

  let show_state = Rep.show_state

  let name = Rep.name

  let re_sign input =
    match input with
    | NetProcess.Input.Raw _ ->
      failwith "huxiang/replica/make/re_sign: input message not signed"
    | NetProcess.Input.Signed { data } ->
      let data = Crypto.Signed.unpack data in
      let data = Crypto.Signed.pack data (module Types.HuxiangBytes) (module Cred) in
      NetProcess.Input.Signed { data }

  let pre (input : NetProcess.Input.t) =
    match input with
    | NetProcess.Input.Signed { data } ->
      let pkey = Crypto.Signed.signer data in
      let originator_in_clique =
        List.exists (fun { Address.owner; _ } ->
            Crypto.Public.equal owner pkey
          ) C.addresses
      in
      if originator_in_clique then
        let reader = I.bin_reader_t in
        let bytes  = Crypto.Signed.unpack data in
        let buffer = Utils.bytes_to_buffer bytes in
        let result = reader.read buffer ~pos_ref:(ref 0) in
        match result with
        | INotification { proof; _ } ->
          if not (Crypto.Public.equal (L.leader proof) pkey) then
            failwith @@ "huxiang/replica/make/pre:"^
                        "player "^(Crypto.Public.show pkey)^
                        "tries to impersonate player "^
                        (Crypto.Public.show (L.leader proof))
          else
            result
        | _ -> result
      else
        IInput (re_sign input)
    | NetProcess.Input.Raw _ ->
      failwith @@ "huxiang/replica/make/pre: "^
                  "input message is not signed"

    let post (output : Rep.output) =
      match output.msg with
      | ONotification _ ->
        let buf   = Bin_prot.Utils.bin_dump O.bin_writer_t output.msg in
        let bytes = Utils.buffer_to_bytes buf in
        { output with Address.msg = bytes }
      | OOutput bytes ->
        { output with msg = bytes }
        
    let thread =
      Process.(postcompose (precompose Rep.thread pre) post)
end
