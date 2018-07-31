module type S =
sig
  val public_key : Types.public_key
  val sign : Bytes.t -> Bytes.t
end
