module Make (P : Types.Process)  :
sig

  val start_mcast : ingoing:string list -> outgoing:string list -> unit

  val start_dynamic :
    ingoing:string list ->
    out_dispatch:(P.O.t -> string) ->
    unit  
  
end
