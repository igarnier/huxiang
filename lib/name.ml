type t =
  | ProcName_Atom of string
  | ProcName_Prod of t list
  | ProcName_Intr of t
[@@deriving show, eq]

let atom x  = ProcName_Atom x
let prod x  = ProcName_Prod x
let inter x = ProcName_Intr x
