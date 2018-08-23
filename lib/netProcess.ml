type input =
  {
    (* route : Address.access_path; *)
    data  : data;
  }

and data =
  | Signed of { data : Bytes.t; pkey : Types.PublicKey.t }
  | Raw of { data : Bytes.t }

type output = Bytes.t Address.multi_dest

let equal_data d1 d2 =
  match d1, d2 with
  | Signed { data = data1; pkey = pk1 }, Signed { data = data2; pkey = pk2 } ->
    Bytes.equal data1 data2 &&
    Types.PublicKey.equal pk1 pk2
  | Raw { data = data1 }, Raw { data = data2 } ->
    Bytes.equal data1 data2
  | _ ->
    false

let equal_input i1 i2 =
  equal_data i1.data i2.data

let equal_output o1 o2 =
  Address.equal_multi_dest Bytes.equal o1 o2

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
  val deserialize : Types.PublicKey.t option -> Bytes.t -> t
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
    match input.data with
    | Signed { data; pkey } ->
      let spkey = 
        Sodium.Sign.Bytes.to_public_key (Types.PublicKey.to_bytes pkey) 
      in
      let bytes = Sodium.Sign.Bytes.sign_open spkey data in
      D.deserialize (Some pkey) bytes
    | Raw { data } ->
      D.deserialize None data

  let post output =
    {
      output with
      Address.msg = S.serialize output.Address.msg
    }

  let thread =
    Process.(postcompose (precompose P.thread pre) post)

end
