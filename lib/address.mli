type t = 
  { 
    owner : Types.public_key;
    pname : Name.t
  }

type 'a multi_dest = {
  dests : t list;
  msg   : 'a
}

include Types.Showable with type t := t
include Types.Equalable with type t := t


val equal_multi_dest : ('a -> 'a -> bool) ->
  'a multi_dest -> 'a multi_dest -> bool

val show_multi_dest : (Format.formatter -> 'a -> unit) ->
  'a multi_dest -> string

val pp_multi_dest : (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a multi_dest -> unit

(** Syntactic sugar to create addressed output messages 
    (type  Address.multi_dest). *)

(* [msg @ (addr, path)] creates a message { msg; dests = [addr,path]}. *)
val (@.) : 'a -> t -> 'a multi_dest

(* [msg @ dests] creates a message { msg; dests }. *)
val (@+) : 'a -> t list -> 'a multi_dest
