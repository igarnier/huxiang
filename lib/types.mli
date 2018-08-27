(* -------------------------------------------------------------------------- *)

(** Interfaces for common operations. *)

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

(** Type of Bytes serializable with bin_prot. *)
module HuxiangBytes :
sig
  type t = Bytes.t

  include (module type of Bytes with type t := t)
  include Showable with type t := t
  include Bin_prot.Binable.S with type t := t

  val to_buf : t -> Bin_prot.Common.buf
  val from_buf : Bin_prot.Common.buf -> t
end

(* -------------------------------------------------------------------------- *)

(** Type of public identities (e.g. hash of public key). *)
(* module PublicKey :
 * sig
 *   type t
 *   include Bin_prot.Binable.S with type t := t
 *   include Equalable with type t := t
 *   include Showable with type t := t
 *   include Ordered with type t := t
 * 
 *   val make : Bytes.t -> t
 *   val to_bytes : t -> Bytes.t
 * end *)

(* -------------------------------------------------------------------------- *)

(** Type of public identities (e.g. hash of public key). *)

(* type public_key = private Bytes.t
 * 
 * val make_public_key : Bytes.t -> public_key
 * 
 * (\** Pretty printing public keys *\)
 * val pp_public_key : Format.formatter -> public_key -> unit
 * 
 * val show_public_key : public_key -> string
 * 
 * (\** Testing public keys for equality *\)
 * val equal_public_key : public_key -> public_key -> bool
 * 
 * (\** Total order on public keys, inherited from Bytes.t *\)
 * val compare_public_key : public_key -> public_key -> int *)

(** Type of hashs *)
type hash = private Bytes.t

val make_hash : Bytes.t -> hash

val equal_hash : hash -> hash -> bool

module type Hashable =
sig
  type t
  val hash : t -> hash
end
(* -------------------------------------------------------------------------- *)

