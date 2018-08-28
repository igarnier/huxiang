
(** In order to make the coalescent product work, one needs a way to
    designate a "leader" among all parties taking part in the coalescent
    product. A proof of leadership accompanies all meta-transitions
    ("notifications") issued by the leader. Coalesced processes are 
    parameterised by an abstract proof of leadership.

    A proof of leadership should be specific to a state of the process:
    it can't be reused.
*)
module type S =
sig

  (** The abstract type of proofs of leadership. *)
  type t

  (** A proof of leadership should be hashable and equalable, with all the usal 
      robustness assumptions on the hashing functions. We must also be able
      to serialize it. *)
  include Crypto.Hashable with type t := t

  include Types.Equalable with type t := t

  include Types.Showable with type t := t

  include Bin_prot.Binable.S with type t := t

  (** To prevent reuse of proofs of leadership, we make each proof point
      to the hash of the previous one. *)
  val prev : t -> Crypto.Hash.t

  (** The genesis "proof", i.e. the root of the tree of proofs. *)
  val root : t

  (** [extend proof k] returns [None] if the player with public key [k]
      has no right to extend [proof], or [Some proof] in the other case. *)
  val extend : t -> Crypto.Secret.t -> Crypto.Public.t -> t option

  (** Each proof of leadership also points to the public key of the leader. *)
  val leader : t -> Crypto.Public.t

end
