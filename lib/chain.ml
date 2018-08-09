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
    (** Associated data *)
    data  : Data.t list;

    (** Proof of leadership *)
    proof : L.t;

    (** Hash of the proof *)
    hash  : Types.hash
  }

  module Table =
    Map.Make(Bytes)

  type t = {
    nodes   : node Table.t;
    pending : node Table.t;
    head    : node;
    chain   : node Table.t;
  }

  type result =
    | Consistent of t
    | Inconsistent of t * Data.t * Data.t
    | Extended of t
    | Pending of t

  let check_consistency data hash table =
    let node = Table.find hash table.nodes in
    match List.find_opt (not % (Data.consistent data)) node.data with
    | Some inconsistent ->
      Inconsistent(table, data, inconsistent)
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
    if Table.mem (hash :> Bytes.t) table.nodes then
      check_consistency data (hash :> Bytes.t) table
    else
      let prev_hash = L.prev proof in
      let node = { data = [data]; proof; hash } in
      if Types.equal_hash prev_hash table.head.hash then
        extend_chain node table
      else
        let nodes   = Table.add (hash :> Bytes.t) node table.nodes in
        let pending = Table.add (prev_hash :> Bytes.t) node table.pending in
        let table = { table with nodes; pending } in
        Pending table

  and extend_chain node table =
    let nodes = Table.add (node.hash :> Bytes.t) node table.nodes in
    let chain = Table.add (node.hash :> Bytes.t) node table.chain in
    let table = { table with 
                  nodes; chain;
                  head = node } in
    resolve_pending table

  and resolve_pending table =
    let head_hash = table.head.hash in
    if Table.mem (head_hash :> Bytes.t) table.pending then
      let new_node = Table.find (head_hash :> Bytes.t) table.pending in
      let pending  = Table.remove (head_hash :> Bytes.t) table.pending in
      let table   = { table with pending } in
      extend_chain new_node table
    else
      Extended table

  let find key table =
    Table.find key table.nodes
  
end
