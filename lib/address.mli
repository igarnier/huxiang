(** Nodes in the network are entirely identified by a public key and
    a name. At deployment time (see module [Node]), a network map has
    to be provided mapping these values to actual addresses. *)
type t = 
  { 
    owner : Crypto.Public.t;
    pname : Name.t
  }

(** Messages are often multi-cast. Values of type [multi_dest] encapsulate 
    a message [msg] together with a list of destinations (field [dests]). *)
type 'a multi_dest = {
  dests : t list;
  msg   : 'a
}

module type Clique =
sig
  val addresses : t list
end

module type PointedClique =
sig
  include Clique
  val owner : Crypto.Public.t
end

include Types.Showable with type t := t
include Types.Equalable with type t := t
include Crypto.Hashable with type t := t
include Bin_prot.Binable.S with type t := t

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
