
type 'a node = {
  elt   : 'a;
  prev  : Types.hash
}


(* Hashtable whose keys are *)
module Table =
  Hashtbl.Make(struct

    type t = Types.hash
               
    let equal = Types.equal_hash
                  
    let hash h =
      Hashtbl.hash (Types.bytes_of_hash h)
  end)
