module Make (P : Types.Process)  :
sig

  val start_mcast : listening:string -> outgoing:string list -> unit

  (** TODO/!\: allow to dispatch on more than one address... *)
  val start_dynamic : listening:string -> out_dispatch:(P.O.t -> string) -> unit  
  
end
