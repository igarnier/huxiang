open Bin_prot

let map_writer (type t u) (w : t Type_class.writer) (f : u -> t) : u Type_class.writer =
  {
    Type_class.size = (fun v -> w.Type_class.size (f v));
    Type_class.write = (fun buf ~pos v ->
      w.Type_class.write buf ~pos (f v))
  }

let map_reader (type t u) (r : t Type_class.reader) (f : t -> u) : u Type_class.reader =
  {
    Type_class.read = (fun buf ~pos_ref -> f (r.Type_class.read buf ~pos_ref));
    vtag_read = (fun buf ~pos_ref tag -> f (r.vtag_read buf ~pos_ref tag))
  }

let bytes_to_buffer bytes =
  let len = Bytes.length bytes in
  let b = Common.create_buf len in
  Common.blit_bytes_buf bytes b ~len;
  b
  
let buffer_to_bytes buf =
  let len   = Common.buf_len buf in
  let bytes = Bytes.create len in
  Common.blit_buf_bytes buf bytes ~len;
  bytes
