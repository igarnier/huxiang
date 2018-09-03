val map_writer : 'a Bin_prot.Type_class.writer -> ('b -> 'a) -> 'b Bin_prot.Type_class.writer
val map_reader : 'a Bin_prot.Type_class.reader -> ('a -> 'b) -> 'b Bin_prot.Type_class.reader
