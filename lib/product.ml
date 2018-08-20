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

  type state = 
    {
      procs   : (module NetProcess.S) array;
      buffers : buffer array
    }

  let owners  = List.map (fun { Address.owner; _ } -> owner) P.addresses

  let _ =
    (* Sanity check *)
    let owners' = List.sort_uniq Types.compare_public_key owners in
    assert (List.length owners = List.length owners');
    assert (List.exists (Types.equal_public_key P.owner) owners)

  let process_of_owner owner =
    let (_, result) =
      List.fold_left2 (fun (index, res) proc proc_owner ->
          if Types.equal_public_key proc_owner owner then
            (index + 1, Some index)
          else
            (index + 1, res)
        ) (0, None) P.processes owners
    in
    result

  let index_of_my_process = 
    match process_of_owner P.owner with
    | None ->
      failwith @@
      "huxiang/product/index_of_my_process: owner is not in process list"
    | Some i -> i

  let show_state _ = "opaque"

  let key_is_in_clique pkey =
    List.exists (fun owner ->
        Types.equal_public_key owner pkey
      ) owners

  let rec process state =
    Process.with_input (fun { NetProcess.sdata; route; pkey } ->
        if key_is_in_clique pkey then
          (* This input message comes from the inside of the Clique. 
             This should not happen. *)
          Lwt.fail_with @@
          "huxiang/product/process: inbound message from clique, error"
        else
          (* This input message comes from outside of the Clique.
             It must be destinated to
          *)
          failwith "TODO"
    )

  let name = failwith ""

  let thread = {
    Process.move  = process;
    Process.state = { procs   = Array.of_list P.processes; 
                      buffers = Array.make (List.length P.processes) Batteries.Deque.empty
                    }
  }
  
end
