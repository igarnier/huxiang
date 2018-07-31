type input =
  {
    sdata : Bytes.t;
    route : Address.access_path;
    pkey  : Types.public_key
  }

type output = Bytes.t Address.multi_dest

module type S = Process.S with type input = input
                           and type output = output
module type Serializer =
sig
  type t
  val serialize : t -> Bytes.t
end

module type Deserializer =
sig
  type t
  val deserialize : Bytes.t -> Address.access_path -> t
end

module Compile
    (D : Deserializer)
    (S : Serializer)
    (P : Process.S with type input = D.t 
                    and type output = S.t Address.multi_dest)
    : S with type state = P.state