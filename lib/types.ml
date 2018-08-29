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
    let buf = Common.create_buf (length bytes) in
    Common.blit_bytes_buf bytes buf ~len:(length bytes);
    buf

  let from_buf buf =
    let bytes = Bytes.create (Common.buf_len buf) in
    Common.blit_buf_bytes buf bytes ~len:(Common.buf_len buf);
    bytes

end

(* -------------------------------------------------------------------------- *)
