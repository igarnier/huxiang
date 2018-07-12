type json = Yojson.Safe.json

module type Message =
sig

  type t

  val to_yojson : t -> json
  val of_yojson : json -> (t, string) Result.result
  val of_yojson_exn : json -> t

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

