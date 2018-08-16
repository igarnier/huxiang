open Batteries

open Types

module type CheckableType =
sig
  type t
  include Equalable with type t := t
  val consistent : t -> t -> bool
end

module Make(Data : CheckableType)(L : Leadership) =
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
    hash  : Types.hash;

    (** Direct pointer to the previous node. *)
    prev  : node option;
    (** Direct pointer to the next node. *)
    next  : node option
  }

  (** [Table] is a type of map where keys are hashes, concretely encoded
      as byte strings. *)
  module Table =
    struct
      include Map.Make(Bytes)

      let add (hash : Types.hash) v table =
        add (hash :> Bytes.t) v table

      let find (hash : Types.hash) table =
        find (hash :> Bytes.t) table
          
      let mem (hash : Types.hash) table =
        mem (hash :> Bytes.t) table

      let remove (hash : Types.hash) table =
        remove (hash :> Bytes.t) table

      let update (k : Types.hash) (k' : Types.hash) (v : 'a) table =
        update (k :> Bytes.t) (k' :> Bytes.t) v table
    end

  (** [HSet] handles sets of hashes. *)
  module HSet =
    Set.Make(Bytes)

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
    pending : Types.hash Table.t;
    head    : Types.hash;
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
    | Extended of t
    | Pending of t

  (* update prev/next links for nodes that are in the chain. *)
  let relink table =
    let rec loop table node next_node =
      match node.prev with
      | None ->
        let prev_hash = L.prev node.proof in
        let prev_node = Table.find prev_hash table.nodes in
        let node      = { node with prev = Some prev_node; next = next_node } in
        let nodes     = Table.update node.hash node.hash node table.nodes in
        let table     = { table with nodes } in
        loop table prev_node (Some node)
      | Some _ ->
        table
    in
    let head_node = Table.find table.head table.nodes in
    loop table head_node None

  let add_to_existing_node data hash table =
    let node = Table.find hash table.nodes in
    match List.find_opt (not % (Data.consistent data)) node.data with
    | Some inconsistent ->
      Inconsistent(table, data, inconsistent, node.proof)
    | None ->
      let data =
        if List.exists (Data.equal data) node.data then
          node.data
        else
          data :: node.data
      in
      let nodes = Table.update hash hash { node with data } table.nodes in
      Consistent { table with nodes }

  let rec add data proof table =
    let hash = L.hash proof in
    if Table.mem hash table.nodes then
      add_to_existing_node data hash table
    else
      let prev_hash = L.prev proof in
      let node = { data = [data]; proof; hash; prev = None; next = None } in
      if Types.equal_hash prev_hash table.head then
        let nodes = Table.add node.hash node table.nodes in
        let table = { table with nodes } in
        extend_chain node table
      else
        let nodes   = Table.add hash node table.nodes in
        let pending = Table.add prev_hash node.hash table.pending in
        let table   = { table with nodes; pending } in
        Pending table

  and extend_chain node table =
    let prev_head = Table.find table.head table.nodes in
    let new_head  = { node with prev = Some prev_head; next = None } in
    let nodes     = Table.update node.hash node.hash new_head table.nodes in
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
      Extended table (* (relink table) *)

  let get_node table hash =
    Table.find hash table.nodes

  (* let get_prev_hash table (hash : Types.hash) =
   *   let node = Table.find hash table.nodes in
   *   L.prev node.proof *)

end
