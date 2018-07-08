type json = Yojson.Safe.json

module type Message_Sig =
sig

  type t

  val to_json : t -> json
  val from_json : json -> t

end

module type ProtocolLwt_Sig =
sig

  module I : Message_Sig
  module O : Message_Sig

  type state

  val initial_state   : state
  val initial_message : O.t

  val transition : state -> I.t -> (state * O.t option) Lwt.t
  
end

module type Protocol_Sig =
sig

  module I : Message_Sig
  module O : Message_Sig

  type state

  val initial_state   : state
  val initial_message : O.t

  val transition : state -> I.t -> (state * O.t option)
  
end
