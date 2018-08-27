open Types

module Make(P : NetProcess.S) :
sig

  type network_map = Address.t -> string

  val start : 
    listening:string -> 
    network_map:network_map ->
    skey:Crypto.Secret.t ->
    pkey:Crypto.Public.t ->
    unit  
  
end
