val map_writer : 'a Bin_prot.Type_class.writer -> ('b -> 'a) -> 'b Bin_prot.Type_class.writer
val map_reader : 'a Bin_prot.Type_class.reader -> ('a -> 'b) -> 'b Bin_prot.Type_class.reader

val bytes_to_buffer : Bytes.t -> Bin_prot.Common.buf
val buffer_to_bytes : Bin_prot.Common.buf -> Bytes.t
