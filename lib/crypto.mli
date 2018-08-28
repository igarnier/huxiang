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

val random_key_pair : unit -> Secret.t * Public.t
val seeded_key_pair : string -> Secret.t * Public.t

val sign : Secret.t -> Bytes.t -> Bytes.t
val sign_open : Public.t -> Bytes.t -> Bytes.t

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

module type Hashable =
sig
  type t
  val hash : t -> Hash.t
end
