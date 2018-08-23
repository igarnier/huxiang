module type Params =
sig
  val processes : (module NetProcess.S) list
  val addresses : Address.t list
  val owner     : Types.public_key
end

module Make(P : Params) : NetProcess.S =
struct

  type input  = NetProcess.input
  type output = NetProcess.output

  type buffer = NetProcess.input Batteries.Deque.t
      
  module Map =
    Batteries.Map.Make(struct
      type t = Types.public_key
                 
      let compare = Types.compare_public_key
    end)

  type proc_state = {
    proc   : (module NetProcess.S);
    buffer : buffer
  }

  type state = proc_state Map.t

  let owners  = List.map (fun { Address.owner; _ } -> owner) P.addresses

  let _ =
    (* Sanity check *)
    let owners' = List.sort_uniq Types.compare_public_key owners in
    assert (List.length owners = List.length owners');
    assert (List.exists (Types.equal_public_key P.owner) owners)

  let get state pkey =
    Map.find pkey state

  let append_to_buffer state pkey data =
    Map.modify pkey (fun procst ->
        { procst with
          buffer = Batteries.Deque.snoc procst.buffer data
        }
      ) state

  let update state pkey procst =
    Map.update pkey pkey procst state

  let set_proc state pkey proc =
    let procst = get state pkey in
    Map.update pkey pkey { procst with proc } state

  let show_state _ = "opaque"

  let key_is_in_clique pkey =
    List.exists (fun owner ->
        Types.equal_public_key owner pkey
      ) owners
      
  let reroute state (output : Bytes.t Address.multi_dest option) =
    match output with
    | None -> state, None
    | Some { Address.msg; dests } ->
      let inside, outside =
        List.partition (fun ({ Address.owner; _}, _) ->
            Map.mem owner state
          ) dests
      in
      (* dispatch message internally *)
      let state =
        List.fold_left (fun state (addr, _) ->
            let data =
              NetProcess.({
                  data  = Raw { data = msg }
                }) in
            append_to_buffer state addr.Address.owner data
          ) state inside
      in
      match outside with
      | [] ->
        state, None
      | dests ->
        state, Some { Address.msg; dests }


  let rec process state =
    let add_input =
      Process.with_input_plain (fun external_input ->
          let state = append_to_buffer state P.owner external_input in
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
    add_input :: playable_transition

  and play_noinput pkey state code =
    Process.without_input_plain begin
      let%lwt output, next = code in
      let state, output = reroute state output in
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
        let state, output = reroute state output in
        let procst = { buffer = tl; proc = next } in
        let state  = update state pkey procst in
        Process.continue_with ?output state process
    end

  let name = failwith ""

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
