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
    Hex.pp fmt (Hex.of_string (Bytes.to_string bytes))

  let show key =
    let bytes = S.Bytes.of_public_key key in
    Hex.show (Hex.of_string (Bytes.to_string bytes))

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
  try Ok (S.Bytes.sign_open pkey bytes)
  with
  | Sodium.Verification_failure ->
    Error `Verification_failure
    

module Hash =
struct

  type t = Types.HuxiangBytes.t
  [@@deriving eq, ord, bin_io]

  let pp fmt (s : t) =
    Hex.pp fmt (Hex.of_string (Bytes.to_string s))

  let show s =
    Hex.show (Hex.of_string (Bytes.to_string s))

  let digest_bytes b =
    let hash = Sodium.Hash.Bytes.digest b in
    Sodium.Hash.Bytes.of_hash hash

  let digest_buf b =
    let hash = Sodium.Hash.Bigbytes.digest b in
    Sodium.Hash.Bytes.of_hash hash

  let to_bytes hash = (hash :> Bytes.t)
  let of_bytes hash = (hash :> t)

end

module Signed =
struct

  type signed_bytes = {
    data   : Types.HuxiangBytes.t;
    signer : Public.t;
  }
  [@@deriving bin_io, eq, show]

  type 'a t = {
    core  : signed_bytes;
    value : 'a
  }
  [@@deriving bin_io, eq, show]

  (* Unused otherwise, the compiler complains; TODO: avoid generation *)
  let _ = show_signed_bytes

  let open_signed_bytes { data; signer } =
    sign_open signer data

  let pack (type t) (value : t) (module Bin : Bin_prot.Binable.S with type t = t) (module Cred : Credentials) =
    let buf   = Bin_prot.Utils.bin_dump Bin.bin_writer_t value in
    let bytes = Utils.buffer_to_bytes buf in
    let sbytes = sign Cred.secret_key bytes in
    {
      core  = { data = sbytes; signer = Cred.public_key };
      value
    }

  let unpack (type u) (x : u t) =
    x.value

  let signer { core; _ } = core.signer

  let bin_shape_t _ =
    bin_shape_signed_bytes

  let bin_size_t _ =
    fun x ->
      bin_size_signed_bytes x.core

  let bin_write_t (_ : 'a Bin_prot.Write.writer) =
    fun buf ~pos { core; _ } ->
      bin_write_signed_bytes  buf ~pos core

  let bin_read_t (reader : 'a Bin_prot.Read.reader) =
    fun buf ~pos_ref ->
      let core = bin_read_signed_bytes buf ~pos_ref in
      match open_signed_bytes core with
      | Ok bytes ->
        let buf = Utils.bytes_to_buffer bytes in
        let r   = ref 0 in
        let v   = reader buf ~pos_ref:r in
        { core; value = v }
      | Error `Verification_failure ->
        failwith "huxiang/crypto/signed/bin_read_t: verification failure"

  let __bin_read_t__ _ _ =
    failwith "huxiang/crypto/signed/__bin_read_t__: should never be called"

  let bin_writer_t (writer : 'a Bin_prot.Type_class.writer) =
    let size  { core; _ } = bin_size_signed_bytes core in
    let write = bin_write_t writer.write in
    (* let write buf ~pos { core; _ } =
     *   bin_write_signed_bytes buf pos core
     * in *)
    {
      Bin_prot.Type_class.size;
      write
    }

  let bin_reader_t (reader : 'a Bin_prot.Type_class.reader) =
    let read = bin_read_t reader.read in
    let vtag_read _ = 
      failwith "huxiang/crypto/signed/bin_reader_t/vtag_read: not implemented"
    in
    {
      Bin_prot.Type_class.read;
      vtag_read
    }

  let bin_t { Bin_prot.Type_class.shape; writer; reader } =
    {
      Bin_prot.Type_class.shape = bin_shape_t shape;
      writer = bin_writer_t writer;
      reader = bin_reader_t reader
    }
    
    
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
  match sign_open pk signed with
  | Ok opened ->
    Bytes.equal raw opened
  | _ ->
    false
