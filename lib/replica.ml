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

  type input = 
    [ `Leader of L.t
    | `Notification of L.t * bool (* TODO: bool=performed a transition or not, should ultimately be transition_name option *)
    | `Input of P.input ]

  type output_data =
    [ `Notification of L.t * bool
    | `Output of Bytes.t ]

  type output = output_data Address.multi_dest

  module Data =
  struct

    type t =
      | NoTransition
      | Transition
      | Blocked
    [@@deriving eq]

    let consistent x y =
      match x, y with
      | Blocked, _
      | _, Blocked -> false
      | _ ->
        equal x y
  end

  module Chain = Chain.Make(Data)(L)

  type buffer = P.input Batteries.Deque.t

  type state =
    {
      proc    : (module LowLevelProcess.S);
      buff    : buffer;
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

  let put_message msg state =
    { state with buff = Batteries.Deque.snoc state.buff msg }

  let validate_leadership state proof data =
    match Chain.add data proof state.chain with
    | Chain.Consistent chain
    | Chain.Extended chain
    | Chain.Pending chain ->
      { state with chain }
    | Chain.Inconsistent(chain, data1, data2, proof) ->
      let public_key = Bytes.to_string (L.leader proof :> Bytes.t) in
      let s = Printf.sprintf "Inconsistency found, guilty part: %s\n" public_key in
      failwith s

  (* TODO:
     I: extend trace or add to waiting pool
     proof needs to point to (previous proof + Nil)
     if (previous proof) top of trace -> extend trace and check whether waiting notifs can be applied
     else if (previous proof) points in the past -> check transition name matches (here, since we have det. machines, nothing to check ...)
     else if (previous proof) not to be found: add to waiting pool
  *)

  (* Invariant: here, state.current matches state.chain.head. *)
  let rec process state =
    Process.with_input begin function
      | `Leader proof ->
        perform_transition proof state
      | `Notification proof ->
        perform_transition proof state
      | `Input i ->
        let state = put_message i state in
        Process.continue_with state process
    end

  and synch state =
    let current = Chain.get_node state.chain state.current in
    match current.next with
    | None ->
      (* up to date - await more input *)
      process
    | Some node ->
      Process.without_input perform_transition

  and perform_transition proof state =
    match Process.evolve state.proc with
    | Process.NoInput code ->
    (* job: pick some playable transition or none, notify clique. 
       concretely:
       If process is NoInput, play. 
       If process is Input and buffer nonempty, play.
       If process is Input and buffer empty, don't play.
    *)
      let%lwt output, next = code in
      let state  = { state with proc = next } in
      let output = rewrap_output output in
      Process.continue_with ?output state
        (notify_transition_to_peers proof true)
    | Process.Input f ->
      (match Batteries.Deque.front state.buff with
       | None ->
         Process.continue_with state
           (notify_transition_to_peers proof false)
       | Some (inp, buff) ->
         let%lwt output, next = f inp in
         let state  = { state with buff; proc = next } in
         let output = rewrap_output output in
         Process.continue_with ?output state
           (notify_transition_to_peers proof true)
      )
    | Stop ->
      Process.continue_with state
        (notify_transition_to_peers proof false)

  and notification_transition proof state =
    let state = validate_leadership state proof () (* Tkind_Notif *) in
    failwith ""

  and notify_transition_to_peers proof transition state =
    Process.without_input
      (let output = broadcast (`Notification(proof, transition)) in
       Process.continue_with ~output state process)
           
end
