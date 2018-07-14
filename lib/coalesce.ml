type identity = Left | Right
[@@deriving show]

module type Selector =
sig
  val selector : identity
end

module Prod
    (Left : Types.Process)
    (Right : Types.Process)
    (Leader : Types.Leadership)
    (S : Selector) =
struct

  let identity = S.selector

  module I =
  struct

    type left_input  = Left.I.t
    [@@deriving eq, yojson]

    type right_input = Right.I.t
    [@@deriving eq, yojson]

    type transition  =
      | LeftTransition  of left_input option
      | RightTransition of right_input option
    [@@deriving eq, yojson]

    type notification = {
      p_o_l      : Leader.t;
      transition : transition
    }
    [@@deriving eq, yojson]

    (* One constructor per party in the coalition +
       one constructor for receiving notification +
       one constructor for being elected leader. *)
    type t =
      | Leader          of Leader.t
      | InputForLeft    of left_input
      | InputForRight   of right_input
      | Notification of notification
    [@@deriving eq, yojson]

    (* Dynamic typing at the interface level... This innocuous looking code is
       key to the compositionality of the machines produced by the functor. 
       There has to be a cleaner way. *)
    let to_yojson m =
      match m with
      (* Extra messages induced by the coalescence. *)
      | Leader _
      | Notification _ -> to_yojson m
      (* Original messages, encoded as before! *)
      | InputForLeft i    -> Left.I.to_yojson i
      | InputForRight i   -> Right.I.to_yojson i

    let of_yojson j =
      match Left.I.of_yojson j with
      | Ok i    -> Ok (InputForLeft i)
      | Error _ ->
        (match Right.I.of_yojson j with
         | Ok i -> Ok (InputForRight i)
         | Error _ ->
           of_yojson j
        )

    let of_yojson_exn j =
      match of_yojson j with
      | Ok v      -> v
      | Error msg ->
        failwith msg

  end

  module O =
  struct

    type left_output   = Left.O.t
    [@@deriving eq, yojson]

    type right_output = Right.O.t
    [@@deriving eq, yojson]


    type notification = I.notification
    [@@deriving eq, yojson]

    type t =
      | OutputFromLeft  of left_output
      | OutputFromRight of right_output
      | Notification of notification
    [@@deriving eq, yojson]

    let to_yojson m =
      match m with
      (* Extra messages induced by the coalescence. *)
      | Notification _ ->
        to_yojson m
      (* Original messages, encoded as before! *)
      | OutputFromLeft o ->
        Left.O.to_yojson o
      | OutputFromRight o ->
        Right.O.to_yojson o

    let of_yojson j =
      match Left.O.of_yojson j with
      | Ok o -> Ok (OutputFromLeft o)
      | _ ->
        (match Right.O.of_yojson j with
         | Ok o -> Ok (OutputFromRight o)
         | _ ->
           of_yojson j)

    let of_yojson_exn j =
      match of_yojson j with
      | Ok v      -> v
      | Error msg ->
        failwith msg

  end

  type ('s, 'i, 'o) player =
    {
      id    : identity;
      state : 's;
      proc  : ('s, 'i, 'o) Types.t;
      buff  : 'i option
    }

  module ProofList = Hashlist.Make(Leader)

  type state =
    {
      left   : (Left.state, Left.I.t, Left.O.t) player;
      right  : (Right.state, Right.I.t, Right.O.t) player;
      proofs : ProofList.t
    }

  let initial_state =
    {
      left =
        {
          id    = Left;
          state = Left.initial_state;
          proc  = Left.process;
          buff  = None
        };

      right =
        {
          id    = Right;
          state = Right.initial_state;
          proc  = Right.process;
          buff  = None
        };

      proofs = 
        match ProofList.cons Leader.root ProofList.empty with
        | None   -> failwith "coalesce/initial_state: bug found"
        | Some l -> l
    }

  let hash_of_string x = Sodium.Hash.Bytes.to_hash (Bytes.of_string x)
  let hash_of_bytes = Sodium.Hash.Bytes.to_hash
  let hash_to_bytes = Sodium.Hash.Bytes.of_hash
    
  let hash_player
      (type state input output)
      (m : (module Types.Process 
             with type state = state 
              and type I.t = input
              and type O.t = output))
      ({ id; state; proc; buff } : (state, input, output) player) =
    let module M = (val m) in
    let id    = id |> show_identity in
    let state = state |> M.show_state in
    let buff  =
      match buff with
      | None   -> "none"
      | Some i -> Printf.sprintf "some(%s)" (M.I.show i)
    in
    hash_of_string (String.concat "" [id; state; buff])

  let hash_state { left; right } =
    let lefth  = hash_player (module Left) left in
    let righth = hash_player (module Right) right in
    hash_of_bytes (Bytes.cat (hash_to_bytes lefth) (hash_to_bytes righth))

  let validate_leadership state proof =
    match ProofList.cons proof state.proofs with
    | None ->
      Lwt.fail_with "coalesce/validate_leadership: proof does not point to current head"
    | Some new_list ->
      let hash_of_state = hash_state state in
      if Leader.check proof hash_of_state then
        Lwt.return { state with proofs = new_list }
      else
        Lwt.fail_with "coalesce/validate_leadership: proof does not check"

  let log = Lwt_log.log_f ~level:Info

  let validate_input
      (type input) (eq : (module Types.Message with type t = input))
      player
      msg =
    let module M = (val eq) in
    if not (Batteries.Option.eq ~eq:M.equal player.buff msg) then
      (Lwt.fail_with @@
       "coalesce/validate_input: incorrect behaviour detected by "^
       (show_identity player.id)^
       "! message in buffer doesn't match notified input for transition")
    else
      Lwt.return ()

  type ('s,'i,'o) play_outcome =
    | Move_output   of ('s,'i,'o) player * 'o
    | Move_nooutput of ('s,'i,'o) player
    | NoMove

  let play_as player =
    match player.proc, player.buff with
    | Input transition, Some msg ->
      let this_id   = show_identity identity in
      let player_id = show_identity player.id in
      log "%s: play %s with input enabled transition" this_id player_id;%lwt
      (* process requires an input and we got one, perform transition *)
      let%lwt state, out_opt, proc =
        transition player.state msg
      in
      let player = { player with state; proc; buff = None } in
      (match out_opt with
       | None ->
         Lwt.return (Move_nooutput player)
       | Some out ->
         Lwt.return (Move_output(player, out)))
    | NoInput transition, _ ->
      let this_id   = show_identity identity in
      let player_id = show_identity player.id in
      log "%s: play %s noinput transition" this_id player_id;%lwt
      (* We don't need to read anything. *)
      let%lwt state, out_opt, proc =
        transition player.state
      in
      let player = { player with state; proc } in
      (match out_opt with
       | None ->
         Lwt.return (Move_nooutput player)
       | Some out ->
         Lwt.return (Move_output(player, out)))
    | _ -> 
      Lwt.return NoMove

  let put_message player msg =
    match player.buff with
    | None ->
      Lwt.return { player with buff = Some msg }
    | Some _ ->
      Lwt.fail_with @@
      "coalesce/put_message: msg for "^
      (show_identity player.id)^
      " received but buffer nonempty"

  let rec process =
    Types.Input begin fun state msg ->
      match msg with
      | I.Leader proof ->
        leader_transition proof state

      | I.InputForLeft msg ->
        log "%s: input for left" (show_identity identity);%lwt
        let%lwt left = put_message state.left msg in
        Lwt.return ({ state with left }, None, process)

      | I.InputForRight msg ->
        log "%s: input for right" (show_identity identity);%lwt
        let%lwt right = put_message state.right msg in
        Lwt.return ({ state with right }, None, process)

      | I.Notification notification ->
        log "%s: notification" (show_identity identity);%lwt
        notification_transition state notification

    end

  and leader_transition proof state =
    let%lwt state = validate_leadership state proof in
    (* We're the leader! Perform some transition. *)
    (* First, try to play as "Left". This is arbitrary (and in fact each
       player could have a different strategy). We save the inputs because
       they are consumed during successful transitions and we still need
       them  for issuing notifications. *)
    let left_input  = state.left.buff in
    let right_input = state.right.buff in
    match%lwt play_as state.left with
    | Move_output(left, output) ->
      let output_s  = Yojson.Safe.to_string (Left.O.to_yojson output) in
      let this_id   = show_identity identity in
      let player_id = show_identity left.id in
      if left.id = identity then
        (log "%s: played as %s, output %s" this_id player_id output_s;%lwt
         let transition = I.LeftTransition left_input in
         let state      = { state with left } in
         let output     = Some (O.OutputFromLeft output) in
         Lwt.return (state, output, notify_transition proof state transition))
      else
        (log "%s: played as %s hence no rights to output %s, reverting"
           this_id player_id output_s;%lwt
         Lwt.return (state, None, process))
    | Move_nooutput left ->
      let this_id   = show_identity identity in
      let player_id = show_identity left.id in
      log "%s: played as %s, no output" this_id player_id;%lwt
      let transition   = I.LeftTransition left_input in
      let state        = { state with left } in
      Lwt.return (state, None, notify_transition proof state transition)
    | NoMove ->
      (match%lwt play_as state.right with
       | Move_output(right, output) ->
         let this_id   = show_identity identity in
         let player_id = show_identity right.id in
         log "%s: played as %s" this_id player_id;%lwt
         if state.right.id = identity then
           let transition   = I.RightTransition right_input in
           let state        = { state with right } in
           let output       = Some (O.OutputFromRight output) in
           Lwt.return (state, output, notify_transition proof state transition)
         else
           Lwt.return (state, None, process)
       | Move_nooutput right ->
         log "%s: played as right" (show_identity identity);%lwt
         let transition   = I.RightTransition right_input in
         let state        = { state with right } in
         Lwt.return (state, None, notify_transition proof state transition)
       | NoMove ->
         log "%s: blocked" (show_identity identity);%lwt
         Lwt.return (state, None, process)
      )

  and notify_transition proof state transition =
    let notification = { I.p_o_l = proof; transition } in
    (* issue notification after having performed a transition *)
    NoInput (fun state -> 
        Lwt.return (state, Some (O.Notification notification), process)
      )

  and notification_transition state { I.p_o_l; transition } =
    if not (Leader.check p_o_l (hash_state state)) then
      Lwt.fail_with @@
      "coalesce/notification_transition: incorrect proof of leadership!"
    else match transition with
    | I.LeftTransition msg ->
      (validate_input (module Left.I) state.left msg;%lwt
       match%lwt play_as state.left with
       | NoMove ->
         Lwt.fail_with @@
         "coalesce/notification_transition: incorrect behaviour detected! "^
         "No transition possible for "^(show_identity state.left.id)^
         " while notified otherwise"
       | Move_output(left, output) ->
         if identity = Left then
           Lwt.fail_with @@
           "coalesce/notification_transition: incorrect behaviour detected! "^
           "Partner must have picked a forbidden transition."
         else
           let state = { state with left } in
           Lwt.return (state, None, process)
       | Move_nooutput left ->
         let state = { state with left } in
         Lwt.return (state, None, process)
      )
    | I.RightTransition msg ->
      (validate_input (module Right.I) state.right msg;%lwt
       match%lwt play_as state.right with
       | NoMove ->
         Lwt.fail_with @@
         "coalesce/notification_transition: incorrect behaviour detected! "^
         "No transition possible for "^(show_identity state.right.id)^
         " while notified otherwise"
       | Move_output(right, output) ->
         if identity = Right then
           Lwt.fail_with @@
           "coalesce/notification_transition: incorrect behaviour detected! "^
           "Partner must have picked a forbidden transition."
         else
           let state = { state with right } in
           Lwt.return (state, None, process)
       | Move_nooutput right ->
         let state = { state with right } in
         Lwt.return (state, None, process)
      )

end
