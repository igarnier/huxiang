type t =
  {
    owner : Types.PublicKey.t;
    pname : Name.t
  }
[@@deriving show, eq]

type 'a multi_dest = {
  dests : t list;
  msg   : 'a
}
[@@deriving show, eq]

let (@.) msg dest =
  { msg; dests = [dest] }

let (@+) msg dests =
  { msg; dests }
