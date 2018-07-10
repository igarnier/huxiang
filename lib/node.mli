module Make (P : Types.Process)  :
sig

  val start : ingoing:string list -> outgoing:string list -> unit
  
end
