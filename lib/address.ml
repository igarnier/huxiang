type t =
  {
    owner : Types.public_key;
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
