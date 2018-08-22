(* -------------------------------------------------------------------------- *)

type public_key = Bytes.t

let make_public_key x = x

let pp_public_key fmt pkey =
  (* let bytes = Sodium.Sign.Bytes.of_public_key pkey in *)
  Format.pp_print_string fmt (Bytes.to_string pkey)

let show_public_key pkey =
  (* let bytes = Sodium.Sign.Bytes.of_public_key pkey in *)
  Bytes.to_string pkey (* bytes *)

let equal_public_key = Bytes.equal
  (* Sodium.Sign.equal_public_keys *)

let compare_public_key = Bytes.compare

type hash = Bytes.t

let make_hash x = x

let equal_hash = Bytes.equal
  (* Sodium.Hash.equal *)

let bytes_of_hash =
  Sodium.Hash.Bytes.of_hash

(* -------------------------------------------------------------------------- *)

module type Hashable =
sig
  type t
  val hash : t -> hash
end

type json = Yojson.Safe.json

module type Jsonable =
sig
  type t
  val to_yojson : t -> json
  val of_yojson : json -> (t, string) Result.result
end

module type Equalable =
sig
  type t
  val equal : t -> t -> bool
end

module type Showable =
sig
  type t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module type Ordered = Map.OrderedType

(* -------------------------------------------------------------------------- *)

module type Leadership =
sig

  type t

  include Hashable with type t := t
  include Equalable with type t := t
  (* include Jsonable with type t := t *)
  include Showable with type t := t
  include Bin_prot.Binable.S with type t := t

  val prev : t -> hash
  val root : t
  val check : t -> t -> bool
  val leader : t -> public_key

end
