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

type json = Yojson.Safe.json

module type Jsonable =
sig
  type t
  val to_yojson : t -> json
  val of_yojson : json -> (t, string) Result.result
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
(* Public keys *)

(* module PublicKey :
 * sig
 *   type t = Bytes.t
 *   include Bin_prot.Binable.S with type t := t
 *   include Equalable with type t := t
 *   include Showable with type t := t
 *   include Ordered with type t := t
 * 
 *   val make : Bytes.t -> t
 *   val to_bytes : t -> Bytes.t
 * end
 * =
 * struct
 * 
 *   include HuxiangBytes
 *       
 *   let make x = x
 *   let to_bytes x = x
 * end *)

(* type public_key = Bytes.t
 * 
 * let make_public_key x = x
 * 
 * let pp_public_key fmt pkey =
 *   (\* let bytes = Sodium.Sign.Bytes.of_public_key pkey in *\)
 *   Format.pp_print_string fmt (Bytes.to_string pkey)
 * 
 * let show_public_key pkey =
 *   (\* let bytes = Sodium.Sign.Bytes.of_public_key pkey in *\)
 *   Bytes.to_string pkey (\* bytes *\)
 * 
 * let equal_public_key = Bytes.equal
 *   (\* Sodium.Sign.equal_public_keys *\)
 * 
 * let compare_public_key = Bytes.compare *)

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
