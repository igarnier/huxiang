type json = Yojson.Safe.json


(* -------------------------------------------------------------------------- *)

(** Interfaces for common operations. *)

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

(* -------------------------------------------------------------------------- *)

(** Type of public keys. *)

type public_identity = Bytes.t
[@@deriving eq, show]

(* -------------------------------------------------------------------------- *)

(** Type of messages being communicated between state machines. *)
module type Message =
sig

  type t

  (* include Jsonable  with type t := t *)
  include Equalable with type t := t
  include Showable  with type t := t

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

  val elected : unit -> t Lwt.t

  (** To prevent reuse of proofs of leadership, we make each proof point
      to the hash of the previous one. *)
  val prev : t-> Sodium.Hash.hash

  (** The genesis "proof", i.e. the root of the tree of proofs. *)
  val root : t

  (** Validity ("appendability") of a proof depends on the satisfaction of a
      predicate which depends on some hash (typically, the hash of the state 
      of some  process). *)
  val check : t -> t -> bool

end
