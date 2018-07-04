module Make (P : Types.Protocol_Sig)  :
sig

  val start : port:int -> log_callback:(string -> unit Lwt.t) -> initial_message:P.O.t -> keepalive_dt:float -> unit
  
end
