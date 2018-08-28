type t = ()
[@@deriving show,eq]

let hash () =
  Sodium.Hash.Bytes.digest (Bytes.of_string "")

let equal i j = i = j

let prev i = hash ()

let root = ()

let check i state_hash = true

