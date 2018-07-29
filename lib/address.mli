type t = 
  { 
    owner : Types.public_identity;
    pname : Name.t
  }

type access_path =
  | Root
  | Access of Name.t * access_path

type 'a multi_dest = {
  dests : (t * access_path) list;
  msg   : 'a
}

include Types.Showable with type t := t
include Types.Equalable with type t := t

val show_access_path : access_path -> string
val equal_access_path : access_path -> access_path -> bool

val equal_multi_dest : ('a -> 'a -> bool) ->
  'a multi_dest -> 'a multi_dest -> bool

val show_multi_dest : (Format.formatter -> 'a -> unit) ->
  'a multi_dest -> string

val pp_multi_dest : (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a multi_dest -> unit

(** Syntactic sugar to create addressed output messages 
    (type  Address.multi_dest). *)

(* [msg @ addr] creates a message { msg; dests = [addr,Root]}. *)
val (@) : 'a -> t -> 'a multi_dest

(* [msg @ (addr, path)] creates a message { msg; dests = [addr,path]}. *)
val (@.) : 'a -> t * access_path -> 'a multi_dest

(* [msg @ dests] creates a message { msg; dests }. *)
val (@+) : 'a -> (t * access_path) list -> 'a multi_dest

(** Syntactic sugar for access paths.*)
val (-->) : Name.t -> access_path -> access_path

