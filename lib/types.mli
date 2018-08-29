(* -------------------------------------------------------------------------- *)

(** Interfaces for common operations. *)

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
