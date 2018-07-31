type input =
  {
    sdata : Bytes.t; (* signed ! *)
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
 =
struct

  type nonrec input  = input 
  type nonrec output = output

  type state = P.state

  let show_state = P.show_state

  let name = P.name

  let pre input =
    let pkey  = Sodium.Sign.Bytes.to_public_key (input.pkey :> Bytes.t) in
    let bytes = Sodium.Sign.Bytes.sign_open pkey input.sdata in
    D.deserialize bytes input.route

  let post output =
    {
      output with
      Address.msg = S.serialize output.Address.msg
    }

  let thread =
    Process.(postcompose (precompose P.thread pre) post)

end
