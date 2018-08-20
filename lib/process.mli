(** Vanilla state communicating state machines, suitably polymorphic in 
    underlying state, input and output. State machines are encoded as 
    resumptions. *)

(** Transitions are either requiring an input ([Input]) or requiring no input
    ([NoInput]). A process can also [Stop]. *)
type ('i,'o) transition = private
  | Input of ('i -> 'o)
  (** Requires an external input before proceeding to the next state. *)

  | NoInput of 'o
  (** Doesn't require any input before proceeding to the next state. *)

  | Stop
  (** Final state of the process. *)


(** The state of a machine is in fact divided in two parts: the usal part 
    (component [state]) and the input/noinput state of the process. *)
type ('s,'i,'o) t = {
  move  : ('s, 'i, 'o) transition_function;
  state : 's
}

and ('s, 'i, 'o) transition_function =
  's -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition list

and ('s, 'i, 'o) outcome =
  {
    output : 'o option;
    next   : ('s, 'i, 'o) t
  }

(** Processes are state machines with specified messages as inputs and 
    outputs.  *)
module type S =
sig
  
  type input
  type output

  type state

  val show_state : state -> string

  val name   : Name.t

  val thread : (state, input, output) t

end

(* Since the current state of the process is part of the [thread] value,
   a transition from the process corresponds to updating the whole module. *)
type ('a, 'b) process_module = 
  (module S with type input = 'a and type output = 'b)

(** [evolve p q] executes [p] until [Stop], then [q]. *)
val evolve :
  ('a, 'b) process_module ->
  ('a, ('b option * ('a, 'b) process_module) Lwt.t)
    transition list

(* val ( >>> ) :
 *   ('a, 'b, 'c) transition_function ->
 *   ('a, 'b, 'c) transition_function ->
 *   ('a, 'b, 'c) transition_function *)

val with_input : ('i -> ('s, 'i, 'o) outcome Lwt.t) -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition list

val without_input : ('s, 'i, 'o) outcome Lwt.t -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition list

val stop : 's -> ('s, 'i, 'o) outcome Lwt.t

val continue_with : ?output:'o -> 's -> ('s, 'i, 'o) transition_function -> ('s, 'i, 'o) outcome Lwt.t

(* val sequence : ('s, 'i, 'o) t -> ('t, 'o, 'p) t -> ('s * 't, 'i, 'p) t *)

val precompose : ('s, 'i, 'o) t -> ('j -> 'i) -> ('s, 'j, 'o) t

val postcompose : ('s, 'i, 'o) t -> ('o -> 'p) -> ('s, 'i, 'p) t
