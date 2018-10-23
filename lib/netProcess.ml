module Bp = Bin_prot
open Bp.Std

module Input =
struct

  type t =
    | Signed of { data : bytes Crypto.Signed.t }
    | Raw of { data : Types.HuxiangBytes.t }
  [@@deriving bin_io, eq, show]

end

type input = Input.t
[@@deriving show]

type output = Bytes.t Address.multi_dest

module type S = Process.S with type input = input
                           and type output = output

type 's t = ('s, input, output) Process.t


module type Serializer =
sig
  type t 

  val serializer : t Bp.Type_class.writer
end

module type Deserializer =
sig
  type t
  val deserializer : Crypto.Public.t option -> t Bp.Type_class.reader
end

let compile 
    (type inp) (type outp_data) (type st)
    (deserializer : Crypto.Public.t option -> inp Bp.Type_class.reader)
    (serializer : outp_data Bp.Type_class.writer)
    (module P : Process.S with type input = inp
                           and type output = outp_data Address.multi_dest
                           and type state  = st) =
  let module Result = struct

    type nonrec input  = input
    type nonrec output = output

    type state = P.state

    let show_state = P.show_state

    let name = P.name
                 
    let pre (input : Input.t) =
      match input with
      | Input.Signed { data } ->
        let pkey   = Crypto.Signed.signer data in
        let bytes  = Crypto.Signed.unpack data in
        let buffer = Utils.bytes_to_buffer bytes in
        let reader = deserializer (Some pkey) in
        reader.read buffer ~pos_ref:(ref 0)
      | Input.Raw { data = bytes } ->
        let buffer = Utils.bytes_to_buffer bytes in
        let reader = deserializer None in
        reader.read buffer ~pos_ref:(ref 0)

    let post (output : P.output) =
      let buf   = Bp.Utils.bin_dump serializer output.msg in
      let bytes = Utils.buffer_to_bytes buf in
      {
        output with
        Address.msg = bytes
      }

    let thread =
      Process.(postcompose (precompose P.thread pre) post)

  end in
  (module Result : S with type state = st)
