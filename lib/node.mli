open Types

module Make(P : LowLevelProcess.S)(S : Signer.S) :
sig

  type network_map = Address.t -> string

  val start_dynamic : listening:string -> network_map:network_map -> unit  
  
end

(*
   Node: requires process + deserialization on input + serialization on output

   Replica: 
    Naked process (?) -> Process with input = Leader | (Input|Notif) Auth
                                 and output = (Output|Notif) Auth

   How to lift Replica functor to Node process, just adding info on how to serialize?
   -> is serialization natural? Is it up to Replica to specify how to serialize stuff?

---
   Do we really need to expose address serialization in Node?
   - fact: we do need to know where to send the data, so we at least need node name
   - on input, the alternative would be to let machines explicitly receive messages
     of the form (addres * Bytes.t) and let the deserialization be performed there,
     not in Node.
     -> then, we need to make provenance addresses explicit as inputs of processes

---
   Architecture idea: have 1) generic processes (without even multi_dest?)
                           2) refine this into the maximally informative one
                           3) the usual, programmer-friendly processes are converted to the
                              maximally informative ones using some extra contextual info
                              (comm. structure, signer, etc)
*)
