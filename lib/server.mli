module Make (P : Types.Protocol_Sig)  :
sig

  val start : port:int -> log_callback:(string -> unit) -> unit
  
end
