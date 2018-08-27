module Input :
sig

  type t = { data : data }
  and data =
    | Signed of { data : Types.HuxiangBytes.t; pkey : Crypto.Public.t }
    | Raw of { data : Types.HuxiangBytes.t }

  include Bin_prot.Binable.S with type t := t
  include Types.Equalable with type t := t
end

type input  = Input.t
type output = Bytes.t Address.multi_dest

(* val equal_input : Input.t -> Input.t -> bool
 * val equal_output : output -> output -> bool *)

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
  val deserialize : Crypto.Public.t option -> Bytes.t -> t
end

module Compile
    (D : Deserializer)
    (S : Serializer)
    (P : Process.S with type input = D.t 
                    and type output = S.t Address.multi_dest)
    : S with type state = P.state
