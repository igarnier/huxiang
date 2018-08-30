open Bin_prot

module Input =
struct

  type t = { data : data }
  [@@deriving bin_io, eq, show]
  
  and data =
    | Signed of { data : Types.HuxiangBytes.t; pkey : Crypto.Public.t }
    | Raw of { data : Types.HuxiangBytes.t }
  [@@deriving bin_io, eq, show]

let _ = show_data

end

type input = Input.t
[@@deriving show]

type output = Bytes.t Address.multi_dest

(* let equal_data d1 d2 =
 *   match d1, d2 with
 *   | Input.Signed { data = data1; pkey = pk1 }, Input.Signed { data = data2; pkey = pk2 } ->
 *     Bytes.equal data1 data2 &&
 *     Crypto.Public.equal pk1 pk2
 *   | Input.Raw { data = data1 }, Input.Raw { data = data2 } ->
 *     Bytes.equal data1 data2
 *   | _ ->
 *     false *)

(* let equal_input i1 i2 =
 *   equal_data i1.Input.data i2.Input.data
 * 
 * let equal_output o1 o2 =
 *   Address.equal_multi_dest Bytes.equal o1 o2 *)

module type S = Process.S with type input = input
                           and type output = output

type 's t = ('s, input, output) Process.t


module type Serializer =
sig
  type t 

  val serializer : t Type_class.writer
end

module type Deserializer =
sig
  type t
  val deserializer : Crypto.Public.t option -> t Type_class.reader
end

let compile 
    (type inp) (type outp_data) (type st)
    (deserializer : Crypto.Public.t option -> inp Type_class.reader)
    (serializer : outp_data Type_class.writer)
    (p : (module Process.S with type input = inp
                           and type output = outp_data Address.multi_dest
                           and type state  = st)) =
  let module P : Process.S with type input = inp
                           and type output = outp_data Address.multi_dest
                           and type state  = st  = (val p) in
  let module Result = struct

    type nonrec input  = input
    type nonrec output = output

    type state = P.state

    let show_state = P.show_state

    let name = P.name

    let pre (input : Input.t) =
      match input.Input.data with
      | Input.Signed { data; pkey } ->
        let bytes  = Crypto.sign_open pkey data in
        let buffer =
          let b = Common.create_buf (Bytes.length bytes) in
          Common.blit_bytes_buf bytes b ~len:(Bytes.length bytes);
          b
        in
        let reader = deserializer (Some pkey) in
        reader.read buffer ~pos_ref:(ref 0)
      | Input.Raw { data = bytes } ->
        let buffer =
          let b = Common.create_buf (Bytes.length bytes) in
          Common.blit_bytes_buf bytes b ~len:(Bytes.length bytes);
          b
        in
        let reader = deserializer None in
        reader.read buffer ~pos_ref:(ref 0)

    let post (output : P.output) =
      let buf   = Utils.bin_dump serializer output.msg in
      let len   = Common.buf_len buf in
      let bytes = Bytes.create len in
      Common.blit_buf_bytes buf bytes ~len;
      {
        output with
        Address.msg = bytes
      }

    let thread =
      Process.(postcompose (precompose P.thread pre) post)

  end in
  (module Result : S with type state = st)


(* module Compile
 *     (D : Deserializer)
 *     (S : Serializer)
 *     (P : Process.S with type input = D.t 
 *                     and type output = S.t Address.multi_dest)
 *     : S with type state = P.state
 *  =
 * struct
 * 
 *   type nonrec input  = input 
 *   type nonrec output = output
 * 
 *   type state = P.state
 * 
 *   let show_state = P.show_state
 * 
 *   let name = P.name
 * 
 *   let pre input =
 *     match input.Input.data with
 *     | Input.Signed { data; pkey } ->
 *       let bytes  = Crypto.sign_open pkey data in
 *       let buffer =
 *         let b = Common.create_buf (Bytes.length bytes) in
 *         Common.blit_bytes_buf bytes b ~len:(Bytes.length bytes);
 *         b
 *       in
 *       let reader = D.deserializer (Some pkey) in
 *       reader.read buffer ~pos_ref:(ref 0)
 *     | Input.Raw { data = bytes } ->
 *       let buffer =
 *         let b = Common.create_buf (Bytes.length bytes) in
 *         Common.blit_bytes_buf bytes b ~len:(Bytes.length bytes);
 *         b
 *       in
 *       let reader = D.deserializer None in
 *       reader.read buffer ~pos_ref:(ref 0)
 * 
 *   let post (output : P.output) =
 *     let buf   = Utils.bin_dump S.serializer output.msg in
 *     let len   = Common.buf_len buf in
 *     let bytes = Bytes.create len in
 *     Common.blit_buf_bytes buf bytes ~len;
 *     {
 *       output with
 *       Address.msg = bytes
 *     }
 * 
 *   let thread =
 *     Process.(postcompose (precompose P.thread pre) post)
 * 
 * end *)
