module Make (P : Types.Protocol_Sig)  :
sig

  val start : port:int -> log_callback:(string -> unit) -> initial_message:P.O.t -> unit
  
end
