open Types

(** The type of state machine, suitably polymorphic in underlying state, 
    input and output. State machines are encoded as resumptions. *)

(** Transitions are either requiring an input ([Input]) or requiring no input
    ([NoInput]). A process can also [Stop]. *)
type ('i,'o) transition =
  | Input of ('i -> 'o)
  (** Requires an external input before proceeding to the next state. *)

  | NoInput of 'o
  (** Doesn't require any input before proceeding to the next state. *)

  | Stop
  (** Final state of the process. *)

(** The "state" of a machine is in fact divided in two parts:
    the usal part (component [state]) and the input/noinput
    state of the process. *)
type ('s,'i,'o) t = {
  move  : ('s, 'i, 'o) transition_function;
  state : 's
}

and ('s, 'i, 'o) transition_function =
  's -> ('i, ('s, 'i, 'o) outcome_lwt) transition  

and ('s, 'i, 'o) outcome_lwt = ('s, 'i, 'o) outcome Lwt.t

and ('s, 'i, 'o) outcome =
  {
    output : 'o option;
    next   : ('s, 'i, 'o) t
  }


(** Processes are state machines with specified messages as inputs and 
    outputs.  *)
module type S =
sig

  module I : Message
  module O : Message

  type state

  val show_state : state -> string

  val thread : (state, I.t, O.t) t

end

let evolve
    (type i o) 
    (m : (module S with type I.t = i and type O.t = o)) =
  let module M = (val m) in
  let result = M.thread.move M.thread.state in
  match result with
  | Input f ->
    Input (fun i ->
        let%lwt { output; next } = f i in
        let module R : S with type I.t = i and type O.t = o =  
        struct
          module I = M.I
          module O = M.O
          type state = M.state
          let show_state = M.show_state
          let thread = next
        end
        in
        let res = (module R : S
                    with type I.t = i 
                     and type O.t = o)
        in
        (Lwt.return (output, res))
      )
  | NoInput result ->
    NoInput
      (let%lwt { output; next } = result in
       let module R : S with type I.t = i and type O.t = o =  
       struct
         module I = M.I
         module O = M.O
         type state = M.state
         let show_state = M.show_state
         let thread = next
       end
       in
       let res = (module R : S with type I.t = i and type O.t = o) in
       Lwt.return (output, res))
  | Stop ->
    Stop

(** Executes p until Stop, then q *)  
let rec (>>>) p q =
  fun state ->
    match p state with
    | Input f ->
      Input (fun i ->
          let%lwt { output; next } = f i in
          Lwt.return { output; 
                       next = { move  = next.move >>> q; 
                                state = next.state } }
        )    
    | NoInput res ->
      NoInput (let%lwt { output; next } = res in
               Lwt.return { output; 
                            next = { move  = next.move >>> q; 
                                     state = next.state } })
    | Stop ->   
      q state

let with_input (f : 'i -> ('s, 'i, 'o) outcome_lwt) =
  Input f

let without_input (x : ('s, 'i, 'o) outcome_lwt) =
  NoInput x

let stop state = 
  Lwt.return { output = None; 
               next = { move = (fun _ -> Stop); 
                        state } 
             }

let continue_with ?output state move =
  Lwt.return { output;
               next   = { move; state } }
