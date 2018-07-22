module Make (P : Process.S)  :
sig

  type network_map = Process.Address.t -> string

  (* val start_mcast : listening:address -> outgoing:address list -> unit *)

  val start_dynamic : listening:string -> network_map:network_map -> unit  
  
end
