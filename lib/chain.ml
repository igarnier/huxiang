open Types

module type CheckableType =
sig
  type t
  include Equalable with type t := t
  include Showable  with type t := t
  val consistent : t -> t -> bool
end

module Make(Data : CheckableType)(L : Leadership.S) =
struct

  type node = {
    (** Associated data. All data in a node must be pairwise 
        [Data.consistent]. *)
    data  : Data.t list;

    (** Proof of leadership. *)
    proof : L.t;

    (** Hash of the proof of leadership. Since proofs of leadership points
        to the hash of their ancestor, this acts indirectly as a pointer
        to the previous node. *)
    hash  : Crypto.Hash.t;

    (** Direct pointer to the previous node. *)
    prev  : node option;
    (** Direct pointer to the next node. *)
    next  : node option
  }

  (** [Table] is a type of map where keys are hashes, concretely encoded
      as byte strings. *)
  module Table = Batteries.Map.Make(Crypto.Hash)

  (** [nodes] contains the set of /all/ received data.
      This set can be decomposed as the union of the [pending] and [chain] sets.
      [pending] is a map whose codomain contains all nodes whose proof of leadership 
      points to a not-yet-received proof of leadership, i.e. they point to something 
      we expect to receive at some point. Nodes are indexed by the hash of the proof
      of leadership they point to.
      [chain] contains a chain of nodes, whose head is [head].
    *)
  type t = {
    nodes   : node Table.t;
    pending : Crypto.Hash.t Table.t;
    head    : Crypto.Hash.t;
  }

  (** When adding a data together with a proof of leadership, the result is
      either:
      - a consistent table (i.e. the data inserts consistently in an existing 
      node);
      - an inconsistent table, i.e. an inconsistency has been found, in which
      case we exhibit the pair of conflicting data.
      TODO: we should exhibit the public key to which the public key corresponds to.
      - an extended table, i.e. the data inserts into a new node, which means 
      that the associated proof of leadership is new and points to the current
      head;
      - a pending table, i.e. the proof of leadership associated to the data
      points nowhere in the existing set of nodes.
     *)
  type result =
    | Consistent of t
    | Inconsistent of t * Data.t * Data.t * L.t

  (* update prev/next links for nodes that are in the chain. *)
  let relink table =
    let rec loop table node next_node =
      match node.prev with
      | None ->
        let prev_hash = L.prev node.proof in
        let prev_node = Table.find prev_hash table.nodes in
        let node      = { node with prev = Some prev_node; next = next_node } in
        let nodes     = Table.modify node.hash (fun _ -> node) table.nodes in
        (* let nodes     = Table.update node.hash node.hash node table.nodes in *)
        let table     = { table with nodes } in
        loop table prev_node (Some node)
      | Some _ ->
        table
    in
    let head_node = Table.find table.head table.nodes in
    loop table head_node None

  let add_to_existing_node data hash table =
    let node = Table.find hash table.nodes in
    match List.find_opt Batteries.(not % (Data.consistent data)) node.data with
    | Some inconsistent ->
      Inconsistent(table, data, inconsistent, node.proof)
    | None ->
      let data =
        if List.exists (Data.equal data) node.data then
          node.data
        else
          data :: node.data
      in
      let nodes = Table.modify hash (fun _ -> { node with data }) table.nodes in
      (* let nodes = Table.update hash hash { node with data } table.nodes in *)
      Consistent { table with nodes }

  let insert_data data proof table =
    let hash = L.hash proof in
    if Table.mem hash table.nodes then
      add_to_existing_node data hash table
    else
      failwith "insert_data: given proof of leadership was not found in chain"

  let rec insert_proof proof table =
    let hash = L.hash proof in
    if Table.mem hash table.nodes then
      table
    else
      let prev_hash = L.prev proof in
      let node = { data = []; proof; hash; prev = None; next = None } in
      if Crypto.Hash.equal prev_hash table.head then
        let nodes = Table.add node.hash node table.nodes in
        let table = { table with nodes } in
        extend_chain node table
      else
        let nodes   = Table.add hash node table.nodes in
        let pending = Table.add prev_hash node.hash table.pending in
        let table   = { table with nodes; pending } in
        table

  and extend_chain node table =
    let prev_head = Table.find table.head table.nodes in
    let new_head  = { node with prev = Some prev_head; next = None } in
    let nodes     = Table.modify node.hash (fun _ -> new_head) table.nodes in
    let table     = { table with 
                      nodes;
                      head = new_head.hash } in
    resolve_pending table

  and resolve_pending table =
    if Table.mem table.head table.pending then
      let new_node_hash = Table.find table.head table.pending in
      let pending  = Table.remove table.head table.pending in
      let table    = { table with pending } in
      let new_node = Table.find new_node_hash table.nodes in
      extend_chain new_node table
    else
      table (* (relink table) *)

  let get_node table hash =
    Table.find hash table.nodes

  let create () =
    let genesis = {
      data  = [];
      proof = L.root;
      hash  = L.hash L.root;
      prev  = None;
      next  = None
    } in
    {
      nodes   = Table.singleton genesis.hash genesis;
      pending = Table.empty;
      head    = genesis.hash
    }

  let dump ({ pending; head; _ } as table) =

    let print_data l =
      let items = List.map Data.show l in
      let items = List.fold_left (fun acc elt -> acc ^ ";" ^ elt) "" items in
      "[" ^ items ^ "]"
    in
    let print_node { data; hash; _ } =
      let data = print_data data in
      let hash = Crypto.Hash.show hash in
      Printf.sprintf "hash = %s, data = %s" hash data
    in
    let rec loop count node =
      let s = print_node node in
      Printf.printf "node %d: %s\n" count s;
      match node.prev with
      | None -> ()
      | Some prev_node ->
        loop (count + 1) prev_node
    in
    let head_node = get_node table head in
    Printf.printf "Current chain:\n";
    loop 0 head_node;
    Printf.printf "Pending nodes:\n";
    Table.iter (fun being_pointed_to pending_node_hash ->
        let node = print_node (get_node table pending_node_hash) in
        let tgt  = Crypto.Hash.show being_pointed_to in
        Printf.printf "pending: %s pointing to %s\n" node tgt
      ) pending

end

let%test_unit _ =
  let module Data : CheckableType with type t = int =
  struct
    type t = int
    [@@deriving eq, show]

    let consistent x y =
      (x mod 2) = (y mod 2)
  end
  in
  let module Owner1 = (val Crypto.(key_pair_to_cred (seeded_key_pair "user one"))) in
  let addr1 = Address.{ owner = Owner1.public_key; pname = Name.atom "address1" } in
  let module C : Address.Clique =
  struct
    let addresses = [ addr1 ]
  end
  in
  let module L = Leadership.RoundRobin(C)(Owner1) in
  let module Chain = Make(Data)(L) in

  let chain  = Chain.create () in

  let head   = Chain.get_node chain chain.head in
  let proof1 =
    match L.extend head.proof with
    | None ->
      failwith "error"
    | Some proof ->
      proof
  in
  let chain  = Chain.insert_proof proof1 chain in

  let chain = 
    match Chain.insert_data 42 proof1 chain with
    | Consistent chain -> chain
    | Inconsistent _ ->
      failwith "Error found"
  in
  let chain = 
    match Chain.insert_data 44 proof1 chain with
    | Consistent chain -> chain
    | Inconsistent _ ->
      failwith "Error found"
  in
  let _ = 
    match Chain.insert_data 45 proof1 chain with
    | Consistent _ ->
      failwith "Error found: inconsistent data inserted"
    | Inconsistent _ ->
      ()
  in

  let head   = Chain.get_node chain chain.head in
  let proof2 =
    match L.extend head.proof with
    | None ->
      failwith "error"
    | Some proof ->
      proof
  in
  let chain  = Chain.insert_proof proof2 chain in

  let chain = 
    match Chain.insert_data 1337 proof2 chain with
    | Consistent chain -> chain
    | Inconsistent _ ->
      failwith "Error found"
  in

  let head   = Chain.get_node chain chain.head in
  let proof3 =
    match L.extend head.proof with
    | None ->
      failwith "error"
    | Some proof ->
      proof
  in
  let chain  = Chain.insert_proof proof3 chain in

  let chain = 
    match Chain.insert_data 231 proof3 chain with
    | Consistent chain -> chain
    | Inconsistent _ ->
      failwith "Error found"
  in
  Chain.dump chain
