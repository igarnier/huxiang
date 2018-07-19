type ('i, 'o) transition = private
  | Input of ('i -> 'o)
  | NoInput of 'o
  | Stop

type ('s,'i,'o) t = {
  move  : ('s, 'i, 'o) transition_function;
  state : 's
}

and ('s, 'i, 'o) transition_function =
  's -> ('i, ('s, 'i, 'o) outcome_lwt) transition  

and ('s, 'i, 'o) outcome_lwt = ('s, 'i, 'o) outcome Lwt.t

and ('s, 'i, 'o) outcome =
  {
    output : 'o option;
    next   : ('s, 'i, 'o) t
  }

module type S =
sig
  module I : Types.Message
  module O : Types.Message
  type state
  val show_state : state -> string
  val thread : (state, I.t, O.t) t
end

val evolve :
  (module S with type I.t = 'a and type O.t = 'b) ->
  ('a, ('b option * (module S with type I.t = 'a and type O.t = 'b)) Lwt.t)
    transition

val ( >>> ) :
  ('a, 'b, 'c) transition_function ->
  ('a, 'b, 'c) transition_function ->
  ('a, 'b, 'c) transition_function

val with_input : ('i -> ('s, 'i, 'o) outcome_lwt) -> ('i, ('s, 'i, 'o) outcome_lwt) transition

val without_input : ('s, 'i, 'o) outcome_lwt -> ('i, ('s, 'i, 'o) outcome_lwt) transition

val stop : 's -> ('s, 'i, 'o) outcome_lwt

val continue_with : ?output:'o -> 's -> ('s, 'i, 'o) transition_function -> ('s, 'i, 'o) outcome_lwt

