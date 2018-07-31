open Types

(* type 'a message =
 *   | Auth of { signer    : Types.public_identity;
 *               signature : Types.signature;
 *               digest    : Types.hash;
 *               msg       : 'a
 *             }
 *   | NoAuth of { msg : 'a } *)


module Make(P : LowLevelProcess.S)(L : Leadership) =
struct

  type leader       = [ `Leader of L.t ]
  type notification = [ `Notification of L.t ]

  type input  = [ leader | notification | `Input of P.input ]
  type output = [ notification | `Output of P.input ]

  type trace = tagged_proof list

  and tagged_proof =
    {
      tag   : transition_kind;
      proof : L.t
    }

  and transition_kind =
    | Tkind_Leader  (* of transition_name ... *)
    | Tkind_Notif (* of transition_name ... *)

  module ProofTable =
    Hashtbl.Make(struct
      type t = Sodium.Hash.hash

      let equal = Sodium.Hash.equal

      let hash h =
        Hashtbl.hash (Sodium.Hash.Bytes.of_hash h)
    end)

  (* type trace_state = {
   *   trace     : trace;
   *   wait_pool : (\* table of proofs addressed by the hash of the proof on which they /point/ *\)
   * } *)

  type buffer = P.input Batteries.Deque.t

  type state =
    {
      proc  : (module LowLevelProcess.S);
      buff  : buffer;
      trace : trace
    }


  let name = Name.inter P.name

  let put_message msg state =
    { state with buff = Batteries.Deque.snoc state.buff msg }

  let validate_leadership state proof tkind =
    match state.trace with
    | [] -> [ { tag = tkind; proof } ]
    | { tag; proof } :: tl ->
      (* TODO:
         I: extend trace or add to waiting pool
         proof needs to point to (previous proof + Nil)
         if (previous proof) top of trace -> extend trace and check whether waiting notifs can be applied
         else if (previous proof) points in the past -> check transition name matches (here, since we have det. machines, nothing to check ...)
         else if (previous proof) not to be found: add to waiting pool
      *)
      failwith "not like this"

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
    let state = validate_leadership state proof Tkind_Leader in
    failwith ""

  and notification_transition proof state =
    let state = validate_leadership state proof Tkind_Notif in
    failwith ""

end
