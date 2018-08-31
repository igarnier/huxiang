open Bin_prot

type t =
  {
    owner : Crypto.Public.t;
    pname : Name.t
  }
[@@deriving show, eq, bin_io]

type 'a multi_dest = {
  dests : t list;
  msg   : 'a
}
[@@deriving show, eq]

module type Clique =
sig
  val addresses : t list
end

module type PointedClique =
sig
  include Clique
  val owner : Crypto.Public.t
end

let (@.) msg dest =
  { msg; dests = [dest] }

let (@+) msg dests =
  { msg; dests }

let hash x =
  let buf  = Utils.bin_dump bin_writer_t x in
  Crypto.Hash.digest_buf buf

