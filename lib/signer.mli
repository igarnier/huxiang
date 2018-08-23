module type S =
sig
  val public_key : Types.PublicKey.t
  val sign : Bytes.t -> Bytes.t
end
