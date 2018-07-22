open Printf
open Batteries

module type FaceInfo =
sig
  val left_id  : Types.public_identity
  val right_id : Types.public_identity
  val owner    : Types.public_identity
end

module Prod
    (Left : Process.S)
    (Right : Process.S)
    (Leader : Types.Leadership)
    (Info : FaceInfo) =
struct

  (* Specification of a transition that has been played locally. *)
  type transition  =
    | LeftTransition  of Left.I.t option
    | RightTransition of Right.I.t option
  [@@deriving show, eq]

  (* A [notification] is a transition that has been played locally and a proof
      that the issuing node had the rights to perform it. *)
  type notification_data = 
    {
      (* Proof of leadership. *)
      p_o_l      : Leader.t;
      
      (* Transition to be simulated by peers. *)
      transition : transition
    }
  [@@deriving show, eq]

  type notification = [ `notification of notification_data ]
  [@@deriving show, eq]

  type leader = [ `leader of Leader.t ]
  [@@deriving show, eq]

  type left_input = [ `input_for_left of Left.I.t ]
  [@@deriving show, eq]

  type right_input = [ `input_for_right of Right.I.t ]
  [@@deriving show, eq]

  type product_input =
    [ leader | notification | left_input | right_input ]
  [@@deriving show, eq]

  type left_output = [ `output_from_left of Left.O.t ]
  [@@deriving show, eq]

  type right_output = [ `output_from_right of Right.O.t ]
  [@@deriving show, eq]

  type product_output =
    [ notification | left_output | right_output ]
  [@@deriving show, eq]

  module I =
  struct

    type t = product_input
    [@@deriving eq, show]

    let deserialize bytes access_path =
      match access_path with
      | Process.Address.Root ->
        let result =
          (Marshal.from_bytes (Bytes.of_string bytes) 0 : [leader|notification])
        in
        (result :> product_input)
      | Process.Address.Access(pname, pth) ->
        if Process.Name.equal pname Left.name then
          let result = Left.I.deserialize bytes pth in
          `input_for_left result
        else if Process.Name.equal pname Right.name then
          let result = Right.I.deserialize bytes pth in
          `input_for_right result
        else
          failwith "huxiang/coalesce/I/deserialize: input message refers to unknown process"

  end

  module O =
  struct

    type t = product_output
    [@@deriving eq, show]

    let serialize (msg : t) =
      match msg with
      | (`notification _) ->
        Bytes.to_string (Marshal.to_bytes msg [])
      | `output_from_left o ->
        Left.O.serialize o
      | `output_from_right o ->
        Right.O.serialize o

  end

  module Deque = Batteries.Deque

  type ('i, 'o) player = {
    id   : Types.public_identity;
    proc : (module Process.S
             with type I.t = 'i 
              and type O.t = 'o);
    buff : 'i Deque.t
  }

  let peek_buff { buff } =
    match Deque.front buff with
    | None          -> None
    | Some (elt, _) -> Some elt

  module ProofList = Hashlist.Make(Leader)

  type state = {
    left   : (Left.I.t, Left.O.t) player;
    right  : (Right.I.t, Right.O.t) player;
    proofs : Leader.t list;
  }

  let show_state { left; right; proofs } = "coalesce.show_state is opaque"

  let log = Lwt_log.log_f ~level:Info

  let log_s =
    let owner_s = Types.show_public_identity Info.owner in
    fun s -> Lwt_log.log_f ~level:Info "%s: %s" owner_s s

  let validate_leadership state proof =
    log_s "validating leadership";%lwt
    match state.proofs with
    | [] ->
      Lwt.fail_with @@ 
      "coalesce/validate_leadership: "^
      "empty proof list"
    | hd :: tl ->
      if Leader.check proof hd then
        (log_s "leadership validated";%lwt
         Lwt.return { state with proofs = proof :: state.proofs })
      else
        Lwt.fail_with "coalesce/validate_leadership: proof does not check"

  let validate_input
      (type i o)
      player
      msg =
    let module M = 
      (val player.proc : Process.S with type I.t = i and type O.t = o)
    in
    match msg, Deque.front player.buff with
    | Some msg, Some (elt, _) when M.I.equal elt msg -> 
      Lwt.return ()
    | None, None ->
      Lwt.return ()
    | _ ->
      Lwt.fail_with @@
      "coalesce/validate_input: incorrect behaviour detected by "^
      (Types.show_public_identity player.id)^
      "! message in buffer doesn't match notified input for transition"

  type ('s,'i,'o) play_outcome =
    | Move_output   of ('i,'o) player * 'o Process.Address.multi_dest
    | Move_nooutput of ('i,'o) player
    | NoMove

  let play_as player =
    match Process.evolve player.proc, Deque.front player.buff with
    | Input transition, Some (msg, tail) ->
      let player_id = Types.show_public_identity player.id in
      log_s @@ sprintf "play %s with input enabled transition" player_id;%lwt
      (* process requires an input and we got one, perform transition *)
      let%lwt out_opt, proc = transition msg in
      let player = { player with proc; buff = tail } in
      (match out_opt with
       | None ->
         Lwt.return (Move_nooutput player)
       | Some out ->
         Lwt.return (Move_output(player, out)))
    | NoInput transition, _ ->
      let player_id = Types.show_public_identity player.id in
      log_s @@ sprintf "play %s noinput transition" player_id;%lwt
      (* We don't need to read anything. *)
      let%lwt out_opt, proc = transition in
      let player = { player with proc } in
      (match out_opt with
       | None ->
         Lwt.return (Move_nooutput player)
       | Some out ->
         Lwt.return (Move_output(player, out)))
    | _ -> 
      Lwt.return NoMove

  let put_message player msg =
    let buff = Deque.snoc player.buff msg in
    Lwt.return { player with buff }

  let name = Process.Name.prod [Left.name; Right.name]

  let face_addrs =
    let addr_left'  = { Process.Address.owner = Info.left_id; pname = name } in
    let addr_right' = { Process.Address.owner = Info.right_id; pname = name } in
    [ (addr_left', Process.Address.Root); (addr_right', Process.Address.Root) ]
    
    
  let recompute_target_addresses =
    (* If a dest of the message is in the face, then brodcast to the
       whole face. *)
    let addr_left   = { Process.Address.owner = Info.left_id; pname = Left.name } in
    let addr_right  = { Process.Address.owner = Info.right_id; pname = Right.name } in
    let face        = [ addr_left; addr_right ] in
    fun dests ->
      List.fold_left (fun acc ((addr, pth) as target) ->
          match List.find_opt (Process.Address.equal addr) face with
          | None ->
            (* target of message not in [face], keep current addressing *)
            target :: acc
          | Some { Process.Address.pname = previous_target } ->
            (* target of message found in [face]: dispatch to new face,
               modifying access path. *)
            let pth   = Process.Address.Access(previous_target, pth) in
            let addrs = List.map (fun (addr, _) -> (addr, pth)) face_addrs in
            addrs @ acc
        ) [] dests

  let reroute_original_message (msg : O.t) old_dests =
    let dests = recompute_target_addresses old_dests in
    { Process.Address.msg; dests }

  let rec process state =
    Process.with_input begin function
      | `leader proof ->
        leader_transition proof state

      | `notification notification ->
        log_s "%s: notification";%lwt
        notification_transition notification state

      | `input_for_left msg ->
        log_s "input for left";%lwt
        let%lwt left = put_message state.left msg in
        Process.continue_with { state with left } process

      | `input_for_right msg ->
        log_s "input for right";%lwt
        let%lwt right = put_message state.right msg in
        Process.continue_with { state with right } process

    end

  and leader_transition proof state =
    let%lwt state = validate_leadership state proof in
    (* We're the leader! Perform some transition. *)
    (* First, try to play as "Left". This is arbitrary (and in fact each
       player could have a different strategy). We save the inputs because
       they are consumed during successful transitions and we still need
       them  for issuing notifications. *)
    let left_input  = peek_buff state.left in
    let right_input = peek_buff state.right in
    match%lwt play_as state.left with
    | Move_output(left, output) ->
      let output_s  = Left.O.show output.Process.Address.msg in
      let player_id = Types.show_public_identity left.id in
      if Types.equal_public_identity left.id Info.owner then
        (log_s @@ sprintf "played as %s, output %s" player_id output_s;%lwt
         let transition = LeftTransition left_input in
         let state      = { state with left } in
         let output     = reroute_original_message (`output_from_left output.msg) output.dests in
         Process.continue_with ~output state (notify_transition proof transition))
      else
        (log_s @@ sprintf "played as %s hence no rights to output %s, reverting"
           player_id output_s;%lwt
         Process.continue_with state process
        )
    | Move_nooutput left ->
      let player_id = Types.show_public_identity left.id in
      log_s @@ sprintf "played as %s, no output" player_id;%lwt
      let transition   = LeftTransition left_input in
      let state        = { state with left } in
      Process.continue_with state (notify_transition proof transition)
    | NoMove ->
      (log "%s: could not play as left" (Types.show_public_identity Info.owner);%lwt
       match%lwt play_as state.right with
       | Move_output(right, output) ->
         let output_s  = Right.O.show output.Process.Address.msg in
         let player_id = Types.show_public_identity right.id in         
         if Types.equal_public_identity state.right.id Info.owner then
           let%lwt ()     = log_s @@ sprintf "played as %s" player_id in
           let transition = RightTransition right_input in
           let state      = { state with right } in
           let output     = reroute_original_message (`output_from_right output.msg) output.dests in
           Process.continue_with ~output state (notify_transition proof transition)
         else
           let%lwt () =
             log_s @@ sprintf
               "played as %s hence no rights to output %s, reverting" 
               player_id output_s
           in
           Process.continue_with state process
       | Move_nooutput right ->
         log_s "played as right";%lwt
         let transition   = RightTransition right_input in
         let state        = { state with right } in
         Process.continue_with state (notify_transition proof transition)
       | NoMove ->
         log_s "could not play as right: blocked";%lwt
         Process.continue_with state process
      )

  and notify_transition proof transition = fun state ->
    let msg    = `notification { p_o_l = proof; transition } in
    let output = { Process.Address.msg; dests = face_addrs } in
    (* issue notification after having performed a transition *)
    Process.without_input
      (Process.continue_with ~output state process)

  and notification_transition { p_o_l; transition } state =
    let%lwt state = validate_leadership state p_o_l in
    match transition with
    | LeftTransition msg ->
      (validate_input state.left msg;%lwt
       match%lwt play_as state.left with
       | NoMove ->
         Lwt.fail_with @@
         "coalesce/notification_transition: incorrect behaviour detected! "^
         "No transition possible for "^(Types.show_public_identity state.left.id)^
         " while notified otherwise"
       | Move_output(left, output) ->
         if Types.equal_public_identity left.id Info.owner then
           Lwt.fail_with @@
           "coalesce/notification_transition: incorrect behaviour detected! "^
           "Partner must have picked a forbidden transition."
         else
           let state = { state with left } in
           Process.continue_with state process
       | Move_nooutput left ->
         let state = { state with left } in
         Process.continue_with state process
      )
    | RightTransition msg ->
      (validate_input state.right msg;%lwt
       match%lwt play_as state.right with
       | NoMove ->
         Lwt.fail_with @@
         "coalesce/notification_transition: incorrect behaviour detected! "^
         "No transition possible for "^(Types.show_public_identity state.right.id)^
         " while notified otherwise"
       | Move_output(right, output) ->
         if Types.equal_public_identity right.id Info.owner then
           Lwt.fail_with @@
           "coalesce/notification_transition: incorrect behaviour detected! "^
           "Partner must have picked a forbidden transition."
         else
           let state = { state with right } in
           Process.continue_with state process
       | Move_nooutput right ->
         let state = { state with right } in
         Process.continue_with state process
      )

  let thread = 
    let initial_state =
      {
        left   = { id = Info.left_id; proc  = (module Left); buff  = Deque.empty };
        right  = { id = Info.right_id; proc  = (module Right); buff  = Deque.empty };
        proofs = 
          match ProofList.cons Leader.root ProofList.empty with
          | None   -> failwith "coalesce/initial_state: bug found"
          | Some l -> l
      }
    in
    {
      Process.state = initial_state;
      Process.move  = process
    }


end
