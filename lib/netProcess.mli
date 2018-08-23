type input =
  {
    data  : data;
  }

and data =
  | Signed of { data : Bytes.t; pkey : Types.public_key }
  | Raw of { data : Bytes.t }

type output = Bytes.t Address.multi_dest

val equal_input : input -> input -> bool
val equal_output : output -> output -> bool

module type S = Process.S with type input = input
                           and type output = output

type 's t = ('s, input, output) Process.t

module type Serializer =
sig
  type t
  val serialize : t -> Bytes.t
end

module type Deserializer =
sig
  type t
  val deserialize : Types.public_key option -> Bytes.t -> t
end

module Compile
    (D : Deserializer)
    (S : Serializer)
    (P : Process.S with type input = D.t 
                    and type output = S.t Address.multi_dest)
    : S with type state = P.state
