type t =
  {
    owner : Types.public_identity;
    pname : Name.t
  }
[@@deriving show, eq]

type access_path =
  | Root
  | Access of Name.t * access_path
[@@deriving show, eq]

type 'a multi_dest = {
  dests : (t * access_path) list;
  msg   : 'a
}
[@@deriving show, eq]


let (@) msg addr =
  { msg; dests = [(addr, Root)] }

let (@.) msg dest =
  { msg; dests = [dest] }

let (@+) msg dests =
  { msg; dests }

let (-->) name ap =
  Access(name, ap)
