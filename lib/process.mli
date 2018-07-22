(** The type of state machine, suitably polymorphic in underlying state, 
    input and output. State machines are encoded as resumptions. *)

(** Transitions are either requiring an input ([Input]) or requiring no input
    ([NoInput]). A process can also [Stop]. *)
type ('i,'o) transition = private
  | Input of ('i -> 'o)
  (** Requires an external input before proceeding to the next state. *)

  | NoInput of 'o
  (** Doesn't require any input before proceeding to the next state. *)

  | Stop
  (** Final state of the process. *)


(** The "state" of a machine is in fact divided in two parts:
    the usal part (component [state]) and the input/noinput
    state of the process. *)
type ('s,'i,'o) t = {
  move  : ('s, 'i, 'o) transition_function;
  state : 's
}

and ('s, 'i, 'o) transition_function =
  's -> ('i, ('s, 'i, 'o) outcome_lwt) transition Lwt.t

and ('s, 'i, 'o) outcome_lwt = ('s, 'i, 'o) outcome Lwt.t

and ('s, 'i, 'o) outcome =
  {
    output : 'o option;
    next   : ('s, 'i, 'o) t
  }

(** Process names. *)
module Name :
sig

  type t

  val atom : string -> t
  val prod : t list -> t
  val show : t -> string
  val equal : t -> t -> bool

end

module Address :
sig

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

  type 'a prov = {
    path : access_path;
    msg  : 'a
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

end


(** Processes are state machines with specified messages as inputs and 
    outputs.  *)
module type S =
sig
  module I : sig 
    include Types.Equalable 
    include Types.Showable with type t := t
    val deserialize : string -> Address.access_path -> t     
  end
  module O : sig
    include Types.Equalable 
    include Types.Showable with type t := t
    val serialize : t -> string
  end

  type state
  val show_state : state -> string

  val name   : Name.t
  val thread : (state, I.t, O.t Address.multi_dest) t
end

type ('a, 'b) process_module = (module S with type I.t = 'a and type O.t = 'b)

(** [evolve p q] executes [p] until [Stop], then [q]. *)
val evolve :
  ('a, 'b) process_module ->
  ('a, ('b Address.multi_dest option * ('a, 'b) process_module) Lwt.t)
    transition Lwt.t

val ( >>> ) :
  ('a, 'b, 'c) transition_function ->
  ('a, 'b, 'c) transition_function ->
  ('a, 'b, 'c) transition_function

val with_input : ('i -> ('s, 'i, 'o) outcome_lwt) -> ('i, ('s, 'i, 'o) outcome_lwt) transition Lwt.t

val without_input : ('s, 'i, 'o) outcome_lwt -> ('i, ('s, 'i, 'o) outcome_lwt) transition Lwt.t

val stop : 's -> ('s, 'i, 'o) outcome_lwt

val continue_with : ?output:'o -> 's -> ('s, 'i, 'o) transition_function -> ('s, 'i, 'o) outcome_lwt

