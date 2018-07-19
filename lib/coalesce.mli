(** For now, we handle credentials trivially by explicitly stating the identity
    of the owner of the coalesced process as being "left" or "right". *)
type identity = Left | Right

(** The [Selector] module type simply packs an identity. The intuition is that
    we chose which identity we want to run the coalescent product as. *)
module type Selector =
sig
  val selector : identity
end

(** The product exchanges new messages in addition to the previous ones, 
    corresponding to the [prod_input] and [prod_output] types. *)


(** Specification of a transition that has been played locally. *)
type ('left_in, 'right_in) transition  =
  | LeftTransition  of 'left_in option
  | RightTransition of 'right_in option

(** A [notification] is a transition that has been played locally and a proof
    that the issuing node had the rights to perform it. *)
type  ('leader, 'left_in, 'right_in) notification = 
  {
    (** Proof of leadership. *)
    p_o_l      : 'leader;

    (** Transition to be simulated by peers. *)
    transition : ('left_in, 'right_in) transition
  }

(** Input of a product node. *)
type ('leader, 'left_in, 'right_in) prod_input =
  (** Local node has been elected leader for the new round. *)
  | Leader        of 'leader

  (** Input for the left component. *)
  | InputForLeft  of 'left_in

  (** Input for the right component. *)
  | InputForRight of 'right_in

  (** Notification from a remote leader. *)
  | Notification  of ('leader, 'left_in, 'right_in) notification

(** Output of a product node. *)
type ('leader, 'left_out, 'right_out, 'left_in, 'right_in) prod_output =
  (** Input from the left component. *)
  | OutputFromLeft  of 'left_out

  (** Input from the right component. *)
  | OutputFromRight of 'right_out

  (** Notification of transition performed after having been elected
      leader. *)
  | Notification     of ('leader, 'left_in, 'right_in) notification

(** The coalescent product functor takes two processes as input, called [Left]
    and [Right], a [Leader] module implementing proofs of leadership and an
    identity packed in [S]. It returns a [Huxiang.Types.Process] module. *)
module Prod : functor
  (Left : Process.S)
  (Right : Process.S)
  (Leader : Types.Leadership)
  (S : Selector) -> 
  Process.S
  with type I.t = (Leader.t, Left.I.t, Right.I.t) prod_input
   and type O.t = (Leader.t, Left.O.t, Right.O.t, Left.I.t, Right.I.t) prod_output
