open Bin_prot
open Bin_prot.Std

type t =
  | ProcName_Atom of string
  | ProcName_Prod of t list
  | ProcName_Intr of t
[@@deriving show, eq, bin_io]

let atom x  = ProcName_Atom x
let prod x  = ProcName_Prod x
let inter x = ProcName_Intr x

let hash x =
  let buf  = Utils.bin_dump bin_writer_t x in
  Crypto.Hash.digest_buf buf
