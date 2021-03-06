(** Vanilla state communicating state machines, suitably polymorphic in 
    underlying state, input and output. State machines are encoded as 
    resumptions. *)

(** Transitions are either requiring an input ([Input]) or requiring no input
    ([NoInput]).  *)
type ('i,'o) transition = private
  | Input of ('i -> 'o)
  (** Requires an external input before proceeding to the next state. *)

  | NoInput of 'o
  (** Doesn't require any input before proceeding to the next state. *)


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

(** In order to resolve the non-determinism at run-time, we need to introduce
    schedulers. These return the index (starting from 0) of the transition to
    be played, or None if no transition is playable.*)
type ('s, 'i, 'o) scheduler =
  ('i, ('s, 'i, 'o) outcome Lwt.t) transition list -> 
  int option

(** We provide a few generic schedulers for convenience. *)

(** [first_pick_scheduler] picks the first transition available. Beware, this
    scheduler is /not/ fair. *)
val first_pick_scheduler : ('s, 'i, 'o) scheduler

(** [uniform_random_scheduler] picks a transition uniformly at random. *)
val uniform_random_scheduler : ('s, 'i, 'o) scheduler

(** [eager_random_scheduler] picks a transition uniformly at random among
    the NoInput set. If no such transition is available, it picks a transition
    uniformly at random in the Input set. Beware, this scheduler is /not/
    fair. *)
val eager_random_scheduler : ('s, 'i, 'o) scheduler

module type Scheduler =
sig
  val scheduler : ('s, 'i, 'o) scheduler
end

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

(** [evolve p q] executes [p] until [Stop], then [q]. *)
(* val evolve :
 *   ('a, 'b) process_module ->
 *   ('a, ('b option * ('a, 'b) process_module) Lwt.t)
 *     transition list *)

(** A process evolves by applying its state to its transition function, yielding
    a list of possible transitions. [evolve] performs this application. *)
val evolve :
  ('s, 'i, 'o) t -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition list

(** In some cases, it is useful to keep the internal state of the process
    hidden, in which case we have to use the following [evolve_module] function. *)
(* TODO: get rid of this. *)

type ('a, 'b) process_module = 
  (module S with type input = 'a and type output = 'b)

val evolve_module :
  ('a, 'b) process_module ->
  ('a, ('b option * ('a, 'b) process_module) Lwt.t)
    transition list
  

(* val ( >>> ) :
 *   ('a, 'b, 'c) transition_function ->
 *   ('a, 'b, 'c) transition_function ->
 *   ('a, 'b, 'c) transition_function *)

val with_input_plain : ('i -> ('s, 'i, 'o) outcome Lwt.t) -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition

val without_input_plain : ('s, 'i, 'o) outcome Lwt.t -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition

val with_input : ('i -> ('s, 'i, 'o) outcome Lwt.t) -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition list

val without_input : ('s, 'i, 'o) outcome Lwt.t -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition list

val stop : 's -> ('s, 'i, 'o) outcome Lwt.t

val continue_with : ?output:'o -> 's -> ('s, 'i, 'o) transition_function -> ('s, 'i, 'o) outcome Lwt.t

(* val sequence : ('s, 'i, 'o) t -> ('t, 'o, 'p) t -> ('s * 't, 'i, 'p) t *)

val precompose : ('s, 'i, 'o) t -> ('j -> 'i) -> ('s, 'j, 'o) t

val postcompose : ('s, 'i, 'o) t -> ('o -> 'p) -> ('s, 'i, 'p) t
