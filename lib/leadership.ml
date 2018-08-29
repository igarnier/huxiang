open Bin_prot
open Bin_prot.Std

module type S =
sig

  type t

  include Crypto.Hashable with type t := t

  include Types.Equalable with type t := t

  include Types.Showable with type t := t

  include Bin_prot.Binable.S with type t := t

  val prev : t -> Crypto.Hash.t

  val root : t

  val extend : t -> t option

  val leader : t -> Crypto.Public.t

end

module RoundRobin(Clique : Address.Clique)(Cred : Crypto.Credentials) : S =
struct

  let addresses = Array.of_list Clique.addresses

  type t = {
    addresses : Address.t array;
    current   : int;    
    prev      : Types.HuxiangBytes.t (* Hash of the previous proof signed by the current player. *)
  }
  [@@deriving eq, show, bin_io]

  let prev { addresses; current; prev } =
    let owner = addresses.(current).Address.owner in
    let hash  = Crypto.sign_open owner prev in
    Crypto.Hash.of_bytes hash

  let root = {
    addresses;
    current = -1;
    prev    = Bytes.of_string ""
  }

  let hash x =
    let buf = Utils.bin_dump bin_writer_t x in
    Crypto.Hash.digest_buf buf

  let check proof =
    let { addresses; current; prev } = proof in
    if equal proof root then
      true
    else
      let owner = addresses.(current).Address.owner in
      try ignore (Crypto.sign_open owner prev); true
      with
      | Sodium.Verification_failure -> false

  let extend proof =
    if not (check proof) then
      failwith "leadership/roundrobin/extend: invalid proof";
    let { addresses; current; _ } = proof in
    let len  = Array.length addresses in
    let next = addresses.(current + 1 mod len) in    
    if Crypto.Public.equal next.Address.owner Cred.public_key then
      let prev = Crypto.sign Cred.secret_key (Crypto.Hash.to_bytes (hash proof)) in
      Some {
        addresses;
        current = current + 1 mod len;
        prev
      }
    else
      None

  let leader { addresses; current; _ } =
    addresses.(current).Address.owner
  
end

(* -------------------------------------------------------------------------- *)
(* Tests *)

let%test_unit "Leadersip/hash" =
  let module Owner = (val Crypto.(key_pair_to_cred (random_key_pair ()))) in
  let addr = Address.({ owner = Owner.public_key; pname = Name.atom "address1" }) in
  let module C : Address.Clique =
  struct
    let addresses = [ addr ]
  end
  in
  let module RR = RoundRobin(C)(Owner) in
  ignore (RR.hash RR.root)

let%test _ =
  let module Owner1 = (val Crypto.(key_pair_to_cred (random_key_pair ()))) in
  let module Owner2 = (val Crypto.(key_pair_to_cred (random_key_pair ()))) in
  let addr1    = Address.{ owner = Owner1.public_key; pname = Name.atom "address1" } in
  let addr2    = Address.{ owner = Owner2.public_key; pname = Name.atom "address2" } in
  let module C : Address.Clique =
  struct
    let addresses = [ addr1; addr2 ]
  end
  in
  let module RR1 = RoundRobin(C)(Owner1) in
  match RR1.extend RR1.root with
  | None ->
    false
  | Some extension ->
    (match RR1.extend extension with
     | None -> true
     | _    -> false)
