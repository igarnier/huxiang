open Types

(*
   receive external input X -> notify(recv(X)) to all
   leader -> simulate available transition T -> notify(T,proof) to all
                                             -> send potential output only to myself
          -> no transition available -> notify(nothing,proof) to all
   receive notify(nothing,proof) -> just add proof with data "Nothing"
   receive notify(T,proof) -> add to chain state
                              -> if consistent, not in chain and head: 
                                 simulate T
                                 notify(T, proof) to all
                                 send potential output only to myself
                              -> if consistent, not in chain and not head: pool (send nothing)
                              -> if consistent and already in chain: do nothing
                              -> if inconsistent: 
                                 notify blame
                                 go to blame state
   receive notify(blame,counterexample) -> go to "blame" state
   receive notify(recv(X)) -> add to buffer
*)

module type Clique =
sig
  val addresses : Address.t list
end

module Make(P : LowLevelProcess.S)(L : Leadership)(C : Clique) =
struct

  type notification_kind =
    | Transition
    | NoTransition
  [@@deriving eq]
      
  type notification =
    {
      nkind  : notification_kind;
      inputs : P.input list
    }
    
  let equal_notification n1 n2 =
    equal_notification_kind n1.nkind n2.nkind &&
    List.for_all2 LowLevelProcess.equal_input n1.inputs n2.inputs

  (* TODO: bool=performed a transition or not, should ultimately be 
     transition_name option. *)
  type input = 
    [ `Leader of L.t
    | `Notification of L.t * notification
    | `Input of P.input ]

  type output_data =
    [ `Notification of L.t * notification
    | `Output of Bytes.t ]

  type output = output_data Address.multi_dest

  module Data =
  struct

    type t = notification
    [@@deriving eq]

    let consistent x y = failwith ""
      (* match x, y with
       * | Blocked, _
       * | _, Blocked -> false
       * | _ ->
       *   equal x y *)
  end

  module Chain = Chain.Make(Data)(L)

  type buffer = P.input Batteries.Deque.t

  type state =
    {
      proc    : (module LowLevelProcess.S);
      fbuff   : buffer; (* future buffer *)
      pbuff   : buffer; (* present buffer *)
      chain   : Chain.t;
      current : Types.hash
    }

  let broadcast (msg : output_data) =
    let open Address in
    {
      dests = List.map (fun addr -> (addr, Root)) C.addresses;
      msg
    }

  let rewrap_output (outp : P.output option) : output option =
    match outp with
    | None -> None
    | Some outp ->
      Some { outp 
             with msg = `Output outp.msg
           }

  let name = Name.inter P.name

  let put_message_in_future msg state =
    { state with fbuff = Batteries.Deque.snoc state.fbuff msg }

  let put_message_in_present msg state =
    { state with pbuff = Batteries.Deque.snoc state.pbuff msg }


  let validate_leadership state proof =
    { state with chain = Chain.insert_proof proof state.chain }

  (* TODO:
     I: extend trace or add to waiting pool
     proof needs to point to (previous proof + Nil)
     if (previous proof) top of trace -> extend trace and check whether waiting notifs can be applied
     else if (previous proof) points in the past -> check transition name matches (here, since we have det. machines, nothing to check ...)
     else if (previous proof) not to be found: add to waiting pool
  *)

  let try_to_transition state =
    match Process.evolve state.proc with
    | Process.NoInput code ->
      let%lwt output, next = code in
      let state  = { state with proc = next } in
      let output = rewrap_output output in
      Lwt.return (Some (state, output))
    | Process.Input f ->
      (match Batteries.Deque.front state.pbuff with
       | None -> 
         Lwt.return None
       | Some (inp, pbuff) ->
         let%lwt output, next = f inp in
         let state  = { state with pbuff; proc = next } in
         let output = rewrap_output output in
         Lwt.return (Some (state, output))
      )
    | Stop ->
      Lwt.return None
      

  let rec process state =
    Process.with_input begin function
      | `Leader proof ->        
        let state = validate_leadership state proof in
        let node  = Chain.get_node state.chain state.current in
        Process.continue_with state (synch node.next)
      | `Notification(proof, notif) ->
        let state = validate_leadership state proof in
        let chain =
          match Chain.add_to_existing_node notif (L.hash proof) state.chain with
          | Chain.Consistent chain -> chain
          | Chain.Inconsistent(chain, d1, d2, proof) ->
            let guilty = Bytes.to_string (L.leader proof :> Bytes.t) in
            failwith @@ "Inconsistency detected. Originator: "^guilty
        in
        let state = { state with chain } in
        let node  = Chain.get_node state.chain state.current in
        Process.continue_with state (synch node.next)
      | `Input i ->
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
           let state = 
             { state with pbuff = Batteries.Deque.append_list state.pbuff inputs }
           in
           Process.continue_with state (synch node.next)
         | { nkind = Transition; inputs } :: _ ->
           (match%lwt try_to_transition state with
            | None ->
              (* Inconsistency! *)
              let guilty = Bytes.to_string (L.leader node.proof :> Bytes.t) in
              Lwt.fail_with @@ 
              "Inconsistency detected. Could not perform transition. "^
              "Originator: "^guilty
            | Some (state, output) ->
              let state = 
                { state with pbuff = Batteries.Deque.append_list state.pbuff inputs }
              in
              Process.continue_with ?output state (synch node.next))
         | [] ->
           (* The only way the data is empty is if we're leader. *)
           (match%lwt try_to_transition state with
            | None ->
              let notif = { nkind  = NoTransition;
                            inputs = Batteries.Deque.to_list state.fbuff
                          }
              in
              let chain =
                match Chain.add_to_existing_node notif (L.hash node.proof) state.chain with
                | Chain.Consistent chain -> chain
                | Chain.Inconsistent _ ->
                  failwith "Replica: impossible case. Bug found, please report."
              in
              let state  = { state with
                             chain;
                             pbuff = Batteries.Deque.append state.pbuff state.fbuff;
                             fbuff = Batteries.Deque.empty
                           }
              in
              let output = broadcast (`Notification(node.proof, notif)) in
              Process.continue_with ~output state (synch node.next)
            | Some (state, output) ->
              Process.continue_with ?output state
                (fun state -> Process.without_input begin
                     let notif = { nkind  = Transition;
                                   inputs = Batteries.Deque.to_list state.fbuff
                                 }
                     in
                     let chain =
                       match Chain.add_to_existing_node notif (L.hash node.proof) state.chain with
                       | Chain.Consistent chain -> chain
                       | Chain.Inconsistent _ ->
                         failwith "Replica: impossible case. Bug found, please report."
                     in
                     let state  = { state with
                                    chain;
                                    pbuff = Batteries.Deque.append state.pbuff state.fbuff;
                                    fbuff = Batteries.Deque.empty
                                  }
                     in
                     let output = broadcast (`Notification(node.proof, notif)) in
                   Process.continue_with ~output state (synch node.next)
                end
              )
         )
      )
  end

           
end
