module Make (P : Types.Process)  :
sig

  type address   = string
  type addresses = address list

  type out_dispatch = P.O.t -> address list

  val start_mcast : listening:address -> outgoing:address list -> unit

  val start_dynamic : listening:address -> out_dispatch:out_dispatch -> unit  
  
end
