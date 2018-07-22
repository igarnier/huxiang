(** The [FaceInfo] module type specifies the parties in the face being 
    constructed as well as the owner of the process being built. *)
module type FaceInfo =
sig
  val left_id  : Types.public_identity
  val right_id : Types.public_identity
  val owner    : Types.public_identity
end


(** The coalescent product functor takes two processes as input, called [Left]
    and [Right], a [Leader] module implementing proofs of leadership and an
    identity packed in [S]. It returns a [Huxiang.Types.Process] module. *)
module Prod : functor
  (Left : Process.S)
  (Right : Process.S)
  (Leader : Types.Leadership)
  (Info : FaceInfo) -> 
  Process.S
