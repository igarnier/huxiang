module type Elt =
sig
  type t
    
  val hash : t -> Sodium.Hash.hash

  val prev : t -> Sodium.Hash.hash

end


module Make(E : Elt) =
struct

  type t = E.t list

  let cons elt l =
    match l with
    | [] -> Some [elt]
    | hd :: tl ->
      if Sodium.Hash.equal (E.hash hd) (E.prev elt) then
        Some (elt :: l)
      else
        None

  let empty = []

end

