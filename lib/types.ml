type json = Yojson.Safe.json

(** The type of state machine, suitably polymorphic in underlying state, 
    input and output. State machines are encoded as resumptions. *)
type ('s, 'i, 'o) t =
  | Input   of ('s -> 'i -> ('s * 'o option * ('s, 'i, 'o) t) Lwt.t)
  (** Requires an external input before proceeding to the next state. *)

  | NoInput of ('s -> ('s * 'o option * ('s, 'i, 'o) t) Lwt.t)
  (** Doesn't require any input before proceeding to the next state. *)

module type Hashable =
sig
  type t
  val hash : t -> Sodium.Hash.hash
end

module type Jsonable =
sig
  type t
  val to_yojson : t -> json
  val of_yojson : json -> (t, string) Result.result
end

module type Equalable =
sig
  type t
  val equal : t -> t -> bool
end

module type Showable =
sig
  type t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

(** Type of messages being communicated between state machines. *)
module type Message =
sig

  include Jsonable
  include Equalable with type t := t
  include Showable  with type t := t

end

(** Processes are state machines (type [t]) with an initial state.  *)
module type Process =
sig

  module I : Message
  module O : Message

  type state

  val show_state : state -> string

  val initial_state : state

  val process : (state, I.t, O.t) t

end

(** In order to make the coalescent product work, one needs a way to
    designate a "leader" among all parties taking part in the coalescent
    product. A proof of leadership accompanies all meta-transitions
    ("notifications") issued by the leader. Coalesced processes are 
    parameterised by an abstract proof of leadership.

    A proof of leadership should be specific to a state of the process:
    it can't be reused.
*)
module type Leadership =
sig

  (** The abstract type of proofs of leadership. *)
  type t

  (** A proof of leadership should be hashable and equalable, with all the usal 
      robustness assumptions on the hashing functions. We must also be able
      to serialize it if we want to make the coalescing product iterable. *)
  include Hashable with type t := t

  include Equalable with type t := t

  include Jsonable with type t := t

  include Showable with type t := t

  (** To prevent reuse of proofs of leadership, we make each proof point
      to the hash of the previous one. *)
  val prev : t-> Sodium.Hash.hash

  (** The genesis "proof", i.e. the root of the tree of proofs. *)
  val root : t

  (** Validity ("appendability") of a proof depends on the satisfaction of a
      predicate which depends on some hash (typically, the hash of the state 
      of some  process). *)
  val check : t -> Sodium.Hash.hash -> bool

end
