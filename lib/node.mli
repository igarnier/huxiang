module Make (P : Types.Protocol_Sig)  :
sig

  val start : ingoing:string list -> outgoing:string list -> unit
  
end
