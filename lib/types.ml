type json = Yojson.Safe.json

module type Message =
sig

  type t

  val to_json : t -> json
  val from_json : json -> t

end

module type Process =
sig

  module I : Message
  module O : Message

  type state

  val initial_state   : state
  val initial_message : O.t option

  val transition : state -> I.t -> (state * O.t option) Lwt.t
  
end

type ('a, 'b) sum =
  | Inl of 'a
  | Inr of 'b

