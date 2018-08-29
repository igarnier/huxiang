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
(* Easily serializable bytes *)

module HuxiangBytes =
struct

  type t = Bytes.t
  [@@deriving show]

  open Bin_prot

  include (Bytes : module type of Bytes with type t := t)

  let bin_t        = Std.bin_bytes
  let bin_shape_t  = Std.bin_shape_bytes
  let bin_writer_t = Std.bin_writer_bytes
  let bin_reader_t = Std.bin_reader_bytes
  let bin_read_t   = Std.bin_read_bytes
  let bin_write_t  = Std.bin_write_bytes
  let bin_size_t   = Std.bin_size_bytes
  let __bin_read_t__ = Std.__bin_read_bytes__

  let to_buf bytes =
    Bin_prot.Utils.bin_dump bin_writer_t bytes

  let from_buf buf =
    bin_read_t buf ~pos_ref:(ref 0)

end

(* -------------------------------------------------------------------------- *)
(* tests *)

let%test _ =
  let bytes  = Bytes.of_string "test string" in
  let buf    = HuxiangBytes.to_buf bytes in
  let bytes' = HuxiangBytes.from_buf buf in
  HuxiangBytes.equal bytes bytes'
