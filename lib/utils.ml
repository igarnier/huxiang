open Bin_prot


let map_writer (type t u) (w : t Type_class.writer) (f : u -> t) : u Type_class.writer =
  {
    w with
    Type_class.write = fun buf ~pos v ->
      w.Type_class.write buf ~pos (f v)
  }

let map_reader (type t u) (r : t Type_class.reader) (f : t -> u) : u Type_class.reader =
  {
    Type_class.read = (fun buf ~pos_ref -> f (r.Type_class.read buf ~pos_ref));
    vtag_read = (fun buf ~pos_ref tag -> f (r.vtag_read buf ~pos_ref tag))
  }
