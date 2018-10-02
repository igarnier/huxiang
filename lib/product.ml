module type Params =
sig
  include Address.PointedClique
  val processes : (module NetProcess.S) list
end

module Make(P : Params) : NetProcess.S =
struct

  type input  = NetProcess.input
  type output = NetProcess.output

  type buffer = NetProcess.input Batteries.Deque.t
      
  module Map =
    Batteries.Map.Make(Crypto.Public)

  type proc_state = {
    proc   : (module NetProcess.S);
    buffer : buffer
  }

  type state = proc_state Map.t

  let owners  = List.map (fun { Address.owner; _ } -> owner) P.addresses

  let _ =
    (* Sanity check *)
    let owners' = List.sort_uniq Crypto.Public.compare owners in
    assert (List.length owners = List.length owners');
    assert (List.exists (Crypto.Public.equal P.owner) owners)

  (* let get (state : state) pkey =
   *   Map.find pkey state *)

  let append_to_buffer state pkey data =
    Map.modify pkey (fun procst ->
        { procst with
          buffer = Batteries.Deque.snoc procst.buffer data
        }
      ) state

  let set_proc state pkey proc =
    Map.modify pkey (fun procst -> { procst with proc }) state

  let show_state _ = "opaque"

  (* let key_is_in_clique pkey =
   *   List.exists (fun owner ->
   *       Crypto.Public.equal owner pkey
   *     ) owners *)
      
  let reroute emitter_pkey state (output : Bytes.t Address.multi_dest option) =
    match output with
    | None -> state, None
    | Some { Address.msg; dests } ->
      let inside, outside =
        List.partition (fun { Address.owner; _} ->
            Map.mem owner state
          ) dests
      in
      (* dispatch message internally *)
      let state =
        List.fold_left (fun state addr ->
            let data = NetProcess.Input.Raw { data = msg } in
            append_to_buffer state addr.Address.owner data
          ) state inside
      in
      match outside with
      | [] ->
        state, None
      | dests ->
        if Crypto.Public.equal emitter_pkey P.owner then
          (* let _ = failwith @@ "emitting a message on behalf of "^(Crypto.Public.show emitter_pkey) in *)
          state, Some { Address.msg; dests }
        else
          (* Outbound messages emitted by processes that don't belong to P.owner are
             filtered out. *)
          state, None

  let rec process state =
    let add_input =
      Process.with_input_plain (fun external_input ->
          match external_input with
          | NetProcess.Input.Raw _ ->
            Lwt.fail_with "huxiang/product/process: unsigned external input, error"
          | NetProcess.Input.Signed { data } ->
            let pkey  = Crypto.Signed.signer data in
            let state = append_to_buffer state pkey external_input in
            Process.continue_with state process
        )
    and playable_transition =
      Map.fold (fun pkey { buffer; proc } acc ->
          let buffer_empty = Batteries.Deque.is_empty buffer in
          let ks = Process.evolve_module proc in
          List.fold_left (fun acc transition ->
              match transition with
              | Process.NoInput code -> 
                (play_noinput pkey state code) :: acc
              | Process.Input f   ->
                if buffer_empty then
                  acc
                else
                  (play_input pkey buffer state f) :: acc
            ) acc ks
        ) state []
    in
    (* A transition in the product corresponds to either take an inbound input
       and add it to some buffer, OR
       play a transition. *)
    add_input :: playable_transition

  and play_noinput pkey state code =
    Process.without_input_plain begin
      let%lwt output, next = code in
      let state, output = reroute pkey state output in
      let state  = set_proc state pkey next in
      Process.continue_with ?output state process
    end

  and play_input pkey buffer state f =
    Process.without_input_plain begin
      match Batteries.Deque.front buffer with
      | None ->
        Lwt.fail_with "huxiang/product/play_input: empty buffer, bug found"
      | Some(hd, tl) ->
        let%lwt output, next = f hd in
        let state, output = reroute pkey state output in
        let procst = { buffer = tl; proc = next } in
        let state  = Map.modify pkey (fun _ -> procst) state in
        Process.continue_with ?output state process
    end

  let name =
    let names = List.map (fun m ->
        let module M = (val m : NetProcess.S) in
        M.name
      ) P.processes
    in
    Name.prod names

  let thread = 
    let state =
      List.fold_left2 (fun state owner proc ->
          let procst = {
            proc;
            buffer = Batteries.Deque.empty
          } in
          Map.add owner procst state
        ) Map.empty owners P.processes
    in
    {
      Process.move  = process;
      Process.state
    }
  
end
