module type S =
sig

  type t

  include Types.Hashable with type t := t

  include Types.Equalable with type t := t

  include Types.Showable with type t := t

  include Bin_prot.Binable.S with type t := t

  val prev : t -> Types.hash

  val root : t

  val check : t -> t -> bool

  val leader : t -> Crypto.Public.t

end
