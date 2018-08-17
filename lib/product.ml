module type Processes =
sig
  val processes : (module NetProcess.S) list
  val addresses : Address.t list
end

module Make(Procs : Processes)(Owner : sig val owner : Types.public_key end) : NetProcess.S =
struct

  type input  = NetProcess.input
  type output = NetProcess.output

  type state = 
    {
      procs : (module NetProcess.S) list
    }

  let show_state _ = "opaque"

  let name = failwith ""

  let thread = failwith ""
  
end
