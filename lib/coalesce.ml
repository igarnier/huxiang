open Printf

type identity = Left | Right
[@@deriving show]

module type Selector =
sig
  val selector : identity
end

type ('left_in, 'right_in) transition  =
  | LeftTransition  of 'left_in option
  | RightTransition of 'right_in option
[@@deriving show, yojson, eq]

(** A [notification] is a transition that has been played locally and a proof
    that the issuing node had the rights to perform it. *)
type  ('leader, 'left_in, 'right_in) notification = 
  {
    (** Proof of leadership. *)
    p_o_l      : 'leader;

    (** Transition to be simulated by peers. *)
    transition : ('left_in, 'right_in) transition
  }
[@@deriving show, yojson, eq]

(** Input of a product node. *)
type ('leader, 'left_in, 'right_in) prod_input =
  | Leader        of 'leader
  | InputForLeft  of 'left_in
  | InputForRight of 'right_in
  | Notification  of ('leader, 'left_in, 'right_in) notification
[@@deriving show, yojson, eq]

(** Output of a product node. *)
type ('leader, 'left_out, 'right_out, 'left_in, 'right_in) prod_output =
  | OutputFromLeft  of 'left_out
  | OutputFromRight of 'right_out
  | Notification     of ('leader, 'left_in, 'right_in) notification
[@@deriving show, yojson, eq]

module Prod
    (Left : Process.S)
    (Right : Process.S)
    (Leader : Types.Leadership)
    (S : Selector) =
struct

  let identity = S.selector

  module I =
  struct

    type t = (Leader.t, Left.I.t, Right.I.t) prod_input
    [@@deriving eq, yojson, show]

    (* Dynamic typing at the interface level... This innocuous looking code is
       key to the compositionality of the machines produced by the functor. 
       There has to be a cleaner way. *)
    let to_yojson m =
      match m with
      (* Extra messages induced by the coalescence. *)
      | Leader _
      | Notification _  -> to_yojson m
      (* Original messages, encoded as before! *)
      | InputForLeft i  -> Left.I.to_yojson i
      | InputForRight i -> Right.I.to_yojson i

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
      | Ok v    -> v
      | Error s -> failwith s

  end

  module O =
  struct

    type t = (Leader.t, Left.O.t, Right.O.t, Left.I.t, Right.I.t) prod_output
    [@@deriving eq, yojson, show]

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
      | Ok v    -> v
      | Error s -> failwith s

  end

  module Deque = Batteries.Deque

  type ('i, 'o) player = {
    id   : identity;
    proc : (module Process.S
             with type I.t = 'i 
              and type O.t = 'o);
    buff : 'i Deque.t
  }

  (* type ('s, 'i, 'o) player =
   *   {
   *     id    : identity;
   *     state : 's;
   *     proc  : ('s, 'i, 'o) Types.process;
   *     buff  : 'i Deque.t
   *   } *)

  let peek_buff { buff } =
    match Deque.front buff with
    | None          -> None
    | Some (elt, _) -> Some elt

  module ProofList = Hashlist.Make(Leader)

  type state = {
    left   : (Left.I.t, Left.O.t) player;
    right  : (Right.I.t, Right.O.t) player;
    proofs : ProofList.t
  }

  (* type state =
   *   {
   *     left   : (Left.state, Left.I.t, Left.O.t) player;
   *     right  : (Right.state, Right.I.t, Right.O.t) player;
   *     proofs : ProofList.t
   *   } *)

  (* type input  = I.t
   * type output = O.t
   * 
   * type t = (state, input, output) Process.t *)

  let show_state { left; right; proofs } = "coalesce.show_state is opaque"


  let hash_of_string x = Sodium.Hash.Bytes.digest (Bytes.of_string x)
  let hash_of_bytes = Sodium.Hash.Bytes.digest
  let hash_to_bytes = Sodium.Hash.Bytes.of_hash
    
  let hash_player
      (type state input output)
      ({ id; proc; buff } : (input, output) player) =
    let module M = (val proc) in
    let id    = id |> show_identity in
    let state = M.show_state M.thread.state in
    let buff  = 
      let ls = buff  |> Deque.map M.I.show |> Deque.to_list in
      "("^(String.concat "," ls)^")"
    in
    hash_of_string (String.concat "" [id; state; buff])

  let hash_state { left; right } =
    let lefth  = hash_player left in
    let righth = hash_player right in
    hash_of_bytes (Bytes.cat (hash_to_bytes lefth) (hash_to_bytes righth))

  let log = Lwt_log.log_f ~level:Info

  let log_s s = Lwt_log.log_f ~level:Info "%s: %s" (show_identity identity) s

  let validate_leadership state proof =
    log_s "validating leadership";%lwt
    match ProofList.cons proof state.proofs with
    | None ->
      Lwt.fail_with "coalesce/validate_leadership: proof does not point to current head"
    | Some new_list ->
      let hash_of_state = hash_state state in
      if Leader.check proof hash_of_state then
        (log_s "leadership validated";%lwt
         Lwt.return { state with proofs = new_list })
      else
        Lwt.fail_with "coalesce/validate_leadership: proof does not check"

  let validate_input
      (type i o)
      player
      msg =
    let module M = (val player.proc : Process.S with type I.t = i and type O.t = o) in
    match msg, Deque.front player.buff with
    | Some msg, Some (elt, _) when M.I.equal elt msg -> 
      Lwt.return ()
    | None, None ->
      Lwt.return ()
    | _ ->
      Lwt.fail_with @@
      "coalesce/validate_input: incorrect behaviour detected by "^
      (show_identity player.id)^
      "! message in buffer doesn't match notified input for transition"

  type ('s,'i,'o) play_outcome =
    | Move_output   of ('i,'o) player * 'o
    | Move_nooutput of ('i,'o) player
    | NoMove

  let play_as player =
    match Process.evolve player.proc, Deque.front player.buff with
    | Input transition, Some (msg, tail) ->
      let player_id = show_identity player.id in
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
      let player_id = show_identity player.id in
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

  let rec process state =
    Process.with_input begin function
      | Leader proof ->
        leader_transition proof state

      | InputForLeft msg ->
        log_s "input for left";%lwt
        let%lwt left = put_message state.left msg in
        Process.continue_with { state with left } process

      | InputForRight msg ->
        log_s "input for right";%lwt
        let%lwt right = put_message state.right msg in
        Process.continue_with { state with right } process

      | Notification notification ->
        log_s "%s: notification";%lwt
        notification_transition notification state

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
      let output_s  = Yojson.Safe.to_string (Left.O.to_yojson output) in
      let player_id = show_identity left.id in
      if left.id = identity then
        (log_s @@ sprintf "played as %s, output %s" player_id output_s;%lwt
         let transition = LeftTransition left_input in
         let state      = { state with left } in
         let output     = OutputFromLeft output in
         Process.continue_with ~output state (notify_transition proof transition))
      else
        (log_s @@ sprintf "played as %s hence no rights to output %s, reverting"
           player_id output_s;%lwt
         Process.continue_with state process
        )
    | Move_nooutput left ->
      let player_id = show_identity left.id in
      log_s @@ sprintf "played as %s, no output" player_id;%lwt
      let transition   = LeftTransition left_input in
      let state        = { state with left } in
      Process.continue_with state (notify_transition proof transition)
    | NoMove ->
      (log "%s: could not play as left" (show_identity identity);%lwt
       match%lwt play_as state.right with
       | Move_output(right, output) ->
         let output_s  = Yojson.Safe.to_string (Right.O.to_yojson output) in
         let player_id = show_identity right.id in         
         if state.right.id = identity then
           let%lwt ()       = log_s @@ sprintf "played as %s" player_id in
           let transition   = RightTransition right_input in
           let state        = { state with right } in
           let output       = OutputFromRight output in
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
    let notification = { p_o_l = proof; transition } in
    (* issue notification after having performed a transition *)
    Process.without_input 
      (Process.continue_with ~output:(Notification notification) state process)

  and notification_transition { p_o_l; transition } state =
    let%lwt state = validate_leadership state p_o_l in
    match transition with
    | LeftTransition msg ->
      (validate_input state.left msg;%lwt
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
         "No transition possible for "^(show_identity state.right.id)^
         " while notified otherwise"
       | Move_output(right, output) ->
         if identity = Right then
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
        left  = { id = Left;  proc  = (module Left); buff  = Deque.empty };
        right = { id = Right; proc  = (module Right); buff  = Deque.empty };

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


  (* let rec process =
   *   { 
   *     Process.move = (fun state ->
   *       Process.Input begin fun msg ->
   *           match msg with
   *           | Leader proof ->
   *             leader_transition proof state
   * 
   *           | InputForLeft msg ->
   *             log_s "input for left";%lwt
   *             let%lwt left = put_message state.left msg in
   *             Lwt.return (None, { process with Process.state = { state with left } })
   * 
   *           | InputForRight msg ->
   *             log_s "input for right";%lwt
   *             let%lwt right = put_message state.right msg in
   *             Lwt.return (None, { process with Process.state = { state with right } })
   * 
   *           | Notification notification ->
   *             log_s "%s: notification";%lwt
   *             notification_transition state notification
   *         end);
   * 
   *     state = initial_state
   *   }
   * 
   * and leader_transition proof state =
   *   let%lwt state = validate_leadership state proof in
   *   (\* We're the leader! Perform some transition. *\)
   *   (\* First, try to play as "Left". This is arbitrary (and in fact each
   *      player could have a different strategy). We save the inputs because
   *      they are consumed during successful transitions and we still need
   *      them  for issuing notifications. *\)
   *   let left_input  = peek_buff state.left in
   *   let right_input = peek_buff state.right in
   *   match%lwt play_as state.left with
   *   | Move_output(left, output) ->
   *     let output_s  = Yojson.Safe.to_string (Left.O.to_yojson output) in
   *     let player_id = show_identity left.id in
   *     if left.id = identity then
   *       (log_s @@ sprintf "played as %s, output %s" player_id output_s;%lwt
   *        let transition = LeftTransition left_input in
   *        let state      = { state with left } in
   *        let output     = Some (OutputFromLeft output) in
   *        Lwt.return (output, notify_transition proof state transition))
   *     else
   *       (log_s @@ sprintf "played as %s hence no rights to output %s, reverting"
   *          player_id output_s;%lwt
   *        let result = { process with state } in
   *        Lwt.return (None, result))
   *   | Move_nooutput left ->
   *     let player_id = show_identity left.id in
   *     log_s @@ sprintf "played as %s, no output" player_id;%lwt
   *     let transition   = LeftTransition left_input in
   *     let state        = { state with left } in
   *     Lwt.return (None, notify_transition proof state transition)
   *   | NoMove ->
   *     (log "%s: could not play as left" (show_identity identity);%lwt
   *      match%lwt play_as state.right with
   *      | Move_output(right, output) ->
   *        let output_s  = Yojson.Safe.to_string (Right.O.to_yojson output) in
   *        let player_id = show_identity right.id in         
   *        if state.right.id = identity then
   *          let transition   = RightTransition right_input in
   *          let state        = { state with right } in
   *          let output       = Some (OutputFromRight output) in
   *          let%lwt ()       = log_s @@ sprintf "played as %s" player_id in
   *          Lwt.return (output, notify_transition proof state transition)
   *        else
   *          let%lwt () =
   *            log_s @@ sprintf
   *              "played as %s hence no rights to output %s, reverting" 
   *              player_id output_s
   *          in
   *          let result = { process with state } in
   *          Lwt.return (None, result)
   *      | Move_nooutput right ->
   *        log_s "played as right";%lwt
   *        let transition   = RightTransition right_input in
   *        let state        = { state with right } in
   *        Lwt.return (None, notify_transition proof state transition)
   *      | NoMove ->
   *        log_s "could not play as right: blocked";%lwt
   *        let result = { process with state } in
   *        Lwt.return (None, result)
   *     )
   * 
   * and notify_transition proof state transition =
   *   let notification = { p_o_l = proof; transition } in
   *   (\* issue notification after having performed a transition *\)
   *   {
   *     Process.move = (fun state ->
   *         Process.NoInput
   *           (Lwt.return (Some (Notification notification), { process with state }));
   *       );
   *     state
   *   }
   * 
   * and notification_transition state { p_o_l; transition } =
   *   let%lwt state = validate_leadership state p_o_l in
   *   match transition with
   *   | LeftTransition msg ->
   *     (validate_input state.left msg;%lwt
   *      match%lwt play_as state.left with
   *      | NoMove ->
   *        Lwt.fail_with @@
   *        "coalesce/notification_transition: incorrect behaviour detected! "^
   *        "No transition possible for "^(show_identity state.left.id)^
   *        " while notified otherwise"
   *      | Move_output(left, output) ->
   *        if identity = Left then
   *          Lwt.fail_with @@
   *          "coalesce/notification_transition: incorrect behaviour detected! "^
   *          "Partner must have picked a forbidden transition."
   *        else
   *          let result = { process with state = { state with left } } in
   *          Lwt.return (None, result)
   *      | Move_nooutput left ->
   *        let result = { process with state = { state with left } } in
   *        Lwt.return (None, result)
   *     )
   *   | RightTransition msg ->
   *     (validate_input state.right msg;%lwt
   *      match%lwt play_as state.right with
   *      | NoMove ->
   *        Lwt.fail_with @@
   *        "coalesce/notification_transition: incorrect behaviour detected! "^
   *        "No transition possible for "^(show_identity state.right.id)^
   *        " while notified otherwise"
   *      | Move_output(right, output) ->
   *        if identity = Right then
   *          Lwt.fail_with @@
   *          "coalesce/notification_transition: incorrect behaviour detected! "^
   *          "Partner must have picked a forbidden transition."
   *        else
   *          let result = { process with state = { state with right } } in
   *          Lwt.return (None, result)
   *      | Move_nooutput right ->
   *        let state = { state with right } in
   *        let result = { process with state = { state with right } } in
   *        Lwt.return (None, result)
   *     ) *)

end
