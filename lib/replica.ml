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

  type input  = [ `Leader of L.t
                | `Notification of L.t
                | `Input of P.input ]

  type output = [ `Notification of L.t
                | `Output of P.output ]

  module Data =
  struct
    type t = unit

    let equal _ _ = true

    let consistent _ _ = true
  end

  module Chain = Chain.Make(Data)(L)

  type buffer = P.input Batteries.Deque.t

  type state =
    {
      proc  : (module LowLevelProcess.S);
      buff  : buffer;
      chain : Chain.t
    }


  let name = Name.inter P.name

  let put_message msg state =
    { state with buff = Batteries.Deque.snoc state.buff msg }

  let validate_leadership _ _ _ =
    failwith ""

      (* TODO:
         I: extend trace or add to waiting pool
         proof needs to point to (previous proof + Nil)
         if (previous proof) top of trace -> extend trace and check whether waiting notifs can be applied
         else if (previous proof) points in the past -> check transition name matches (here, since we have det. machines, nothing to check ...)
         else if (previous proof) not to be found: add to waiting pool
      *)

  let rec process state =
    Process.with_input begin function
      | `Leader proof ->
        leader_transition proof state
      | `Notification proof ->
        notification_transition proof state
      | `Input i ->
        let state = put_message i state in
        Process.continue_with state process
    end

  and leader_transition proof state =
    let state = validate_leadership state proof () (* Tkind_Leader *) in
    failwith ""

  and notification_transition proof state =
    let state = validate_leadership state proof () (* Tkind_Notif *) in
    failwith ""

end
