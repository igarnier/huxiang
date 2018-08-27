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

module type S = Process.S with type input = input
                           and type output = output

type 's t = ('s, input, output) Process.t

module type Serializer =
sig
  type t 

  val serializer : t Bin_prot.Type_class.writer
end

module type Deserializer =
sig
  type t
  val deserializer : Crypto.Public.t option -> t Bin_prot.Type_class.reader
end

val compile :
  (Crypto.Public.t option -> 'a Bin_prot.Type_class.reader) ->
  'b Bin_prot.Type_class.writer ->
  (module Process.S 
    with type input = 'a 
     and type output = 'b Address.multi_dest 
     and type state = 'c) ->
  (module S with type state = 'c)
