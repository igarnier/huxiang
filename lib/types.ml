module type Message_Sig =
sig

  type t

  val to_json : t -> Yojson.Safe.json
  val from_json : Yojson.Safe.json -> t

end

module type Protocol_Sig =
sig

  module I : Message_Sig
  module O : Message_Sig

  type state

  val initial_state : state

  val transition : state -> I.t -> (state * O.t option) Lwt.t
  
end
