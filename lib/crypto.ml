module S = Sodium.Sign

module type Key =
sig
  type t

  include Types.Showable with type t := t
  include Types.Equalable with type t := t
  include Types.Ordered with type t := t

  val of_bytes : Bytes.t -> t
  val to_bytes : t -> Bytes.t
end

module Public =
struct

  type t = S.public_key

  let pp fmt key =
    let bytes = S.Bytes.of_public_key key in
    Format.pp_print_string fmt (Bytes.to_string bytes)

  let show key =
    let bytes = S.Bytes.of_public_key key in
    Bytes.to_string bytes

  let equal = S.equal_public_keys

  let compare = S.compare_public_keys

  let to_bytes = S.Bytes.of_public_key

  let of_bytes = S.Bytes.to_public_key

  open Bin_prot

  let bin_size_t s =
    Types.HuxiangBytes.bin_size_t (to_bytes s)

  let bin_write_t : t Bin_prot.Write.writer =
    fun buf ~pos data ->
      Std.bin_write_bytes buf ~pos (to_bytes data)

  let bin_read_t : t Bin_prot.Read.reader =
    fun buf ~pos_ref ->
      of_bytes (Std.bin_read_bytes buf ~pos_ref)

  let bin_writer_t =
    {
      Bin_prot.Type_class.size = bin_size_t;
      write = bin_write_t;
    }

  let bin_reader_t =
    let bytes_reader = Std.bin_reader_bytes in
    {
      Bin_prot.Type_class.read = bin_read_t;
      vtag_read = fun buf ~pos_ref i ->
        of_bytes (bytes_reader.Bin_prot.Type_class.vtag_read buf ~pos_ref i)
    }

  let bin_shape_t = Bin_prot.Shape.bin_shape_bytes

  let bin_t = 
    { Bin_prot.Type_class.shape = bin_shape_t;
      writer = bin_writer_t;
      reader = bin_reader_t
    }

  let __bin_read_t__ =
    Std.__bin_read_bytes__

end

module Secret =
struct

  type t = S.secret_key

  let pp fmt key =
    let bytes = S.Bytes.of_secret_key key in
    Format.pp_print_string fmt (Bytes.to_string bytes)

  let show key =
    let bytes = S.Bytes.of_secret_key key in
    Bytes.to_string bytes

  let equal = S.equal_secret_keys

  let compare _ _ =
    failwith "huxiang/crypto/secret/compare: can't compare secret keys"

  let to_bytes = S.Bytes.of_secret_key
  let of_bytes = S.Bytes.to_secret_key

end

module type Credentials =
sig
  val public_key : Public.t
  val secret_key : Secret.t
end

let random_key_pair () =
  S.random_keypair ()

let seeded_key_pair seed =
  let seed   = Bytes.of_string seed in
  let digest = 
    Sodium.Generichash.Bytes.(
      of_hash (digest ~size:32 seed)
    )
  in
  let seed   = S.Bytes.to_seed digest in
  S.seed_keypair seed

let key_pair_to_cred (secret_key, public_key) =
  (module struct
    let public_key = public_key
    let secret_key = secret_key      
  end : Credentials)

let sign skey bytes =
  S.Bytes.sign skey bytes

let sign_open pkey bytes =
  S.Bytes.sign_open pkey bytes


module Hash =
struct

  type t = Types.HuxiangBytes.t
  [@@deriving show, eq, ord, bin_io]

  let digest_bytes b =
    let hash = Sodium.Hash.Bytes.digest b in
    Sodium.Hash.Bytes.of_hash hash

  let digest_buf b =
    let hash = Sodium.Hash.Bigbytes.digest b in
    Sodium.Hash.Bytes.of_hash hash

  let to_bytes hash = (hash :> Bytes.t)
  let of_bytes hash = (hash :> t)

end

module type Hashable =
sig
  type t
  val hash : t -> Hash.t
end


(* -------------------------------------------------------------------------- *)
(* Tests *)

let%test _ =
  let (sk, pk) = random_key_pair () in
  let raw      = Bytes.of_string "message" in
  let signed   = sign sk raw in
  let opened   = sign_open pk signed in
  Bytes.equal raw opened
