module type Key =
sig
  type t

  include Types.Showable with type t := t
  include Types.Equalable with type t := t
  include Types.Ordered with type t := t

  val of_bytes : Bytes.t -> t
  val to_bytes : t -> Bytes.t
end

module Public : 
  sig 
    include Key
    include Bin_prot.Binable.S with type t := t
  end

module Secret : Key

module type Credentials =
sig
  val public_key : Public.t
  val secret_key : Secret.t
end

val random_key_pair : unit -> Secret.t * Public.t
val seeded_key_pair : string -> Secret.t * Public.t

val key_pair_to_cred : Secret.t * Public.t -> (module Credentials)

val sign : Secret.t -> Bytes.t -> Bytes.t
val sign_open : Public.t -> Bytes.t -> (Bytes.t, [`Verification_failure]) result

module Hash :
sig
  type t

  include Types.Showable with type t := t
  include Types.Equalable with type t := t
  include Types.Ordered with type t := t
  include Bin_prot.Binable.S with type t := t

  val digest_bytes : Bytes.t -> t
  val to_bytes : t -> Bytes.t
  val of_bytes : Bytes.t -> t

  val digest_buf : Bin_prot.Common.buf -> t
end

module Signed :
sig

  include Bin_prot.Binable.S1

  val pack   : 'a -> (module Bin_prot.Binable.S with type t = 'a) -> (module Credentials) -> 'a t
  val unpack : 'a t -> 'a
  val signer : 'a t -> Public.t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val pp : 
    (Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
    Format.formatter -> 'a t -> Ppx_deriving_runtime.unit

  val show :
    (Format.formatter -> 'a -> Ppx_deriving_runtime.unit) ->
    'a t -> Ppx_deriving_runtime.string

end

module type Hashable =
sig
  type t
  val hash : t -> Hash.t
end
