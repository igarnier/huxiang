
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

type round_robin =
  | Proof of {
      (* Hash of the previous proof signed by the current player. *)
      prev      : Crypto.Hash.t Crypto.Signed.t
    }
  | Genesis
[@@deriving eq, show, bin_io]


module RoundRobin(Clique : Address.Clique)(Cred : Crypto.Credentials) : S with type t = round_robin =
struct

  let addresses = Array.of_list Clique.addresses

  type t = round_robin
  [@@deriving eq, bin_io]

  let show = show_round_robin
  let pp   = pp_round_robin

  let index_of_player (pkey : Crypto.Public.t) =
    let len = Array.length addresses in
    let rec loop i =
      if i = len then
        None
      else if Crypto.Public.equal addresses.(i).Address.owner pkey then
        Some i
      else
        loop (i+1)
    in
    loop 0

  let hash x = 
    let buf = Bin_prot.Utils.bin_dump bin_writer_t x in
    Crypto.Hash.digest_buf buf

  let genesis_hash = 
    hash Genesis

  let prev = function
    | Proof { prev; _ } -> Crypto.Signed.unpack prev
    | Genesis           -> genesis_hash

  let root = Genesis

  (* Checks that a proof is well-formed*)
  let check proof =
    match proof with
    | Genesis -> true
    | Proof  { prev } ->
      let signer = Crypto.Signed.signer prev in
      match index_of_player signer with
      | None -> false
      | Some index ->
        let key = addresses.(index).Address.owner in
        Crypto.Public.equal key signer

  let extend proof =
    if not (check proof) then
      failwith "leadership/roundrobin/extend: invalid proof";
    let next =
      match proof with
      | Genesis -> 
        addresses.(0)
      | Proof { prev } ->
        let signer = Crypto.Signed.signer prev in
        match index_of_player signer with
        | None ->
          failwith "leadership/roundrobin/extend: impossible case"
        | Some index ->
          let len = Array.length addresses in
          addresses.((index + 1) mod len)
    in
    if Crypto.Public.equal next.Address.owner Cred.public_key then      
      let signed = 
        Crypto.Signed.pack (hash proof) (module Crypto.Hash) (module Cred) 
      in
      Some (Proof { prev = signed })
    else
      None

  let leader proof =
    match proof with
    | Genesis -> 
      failwith ""
    | Proof { prev } ->
      Crypto.Signed.signer prev
  
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


let%test_unit "Leadersip/hash deterministic" =
  let module Owner = (val Crypto.(key_pair_to_cred (random_key_pair ()))) in
  let addr = Address.({ owner = Owner.public_key; pname = Name.atom "address1" }) in
  let module C : Address.Clique =
  struct
    let addresses = [ addr ]
  end
  in
  let module RR = RoundRobin(C)(Owner) in
  ignore (Crypto.Hash.equal (RR.hash RR.root) (RR.hash RR.root))

let%test "Leadership/extend" =
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
  let module RR2 = RoundRobin(C)(Owner2) in
  match RR1.extend RR1.root, RR2.extend RR2.root with
  | None, Some _ ->
    false
  | Some extension, None ->
    (match RR1.extend extension, RR2.extend extension with
     | None, Some _ -> true
     | _            -> false)
  | _ ->
    false
