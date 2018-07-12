type json = Yojson.Safe.json

module type Message =
sig

  type t

  val to_yojson : t -> json
  val of_yojson : json -> (t, string) Result.result
  val of_yojson_exn : json -> t
  val equal : t -> t -> bool

end


type ('s, 'i, 'o) t =
  | Input   of ('s -> 'i -> ('s * 'o option * ('s, 'i, 'o) t) Lwt.t)
  (** Requires an external input before proceeding to the next state. *)

  | NoInput of ('s -> ('s * 'o option * ('s, 'i, 'o) t) Lwt.t)
  (** Doesn't require any input before proceeding to the next state. *)


(** The module type of processes. *)
module type Process =
sig

  module I : Message
  module O : Message

  type state

  val initial_state : state

  val process : (state, I.t, O.t) t
  
end

(** Generic disjoint sum. *)
type ('a, 'b) sum =
  | Inl of 'a
  | Inr of 'b

