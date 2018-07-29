type t

val atom : string -> t
val prod : t list -> t
val inter : t -> t

include Types.Showable with type t := t
include Types.Equalable with type t := t
