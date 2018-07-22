open Types


type ('i, 'o) transition =
  | Input of ('i -> 'o)
  | NoInput of 'o
  | Stop

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


module Name =
struct

  type t =
    | ProcName_Atom of string
    | ProcName_Prod of t list
  [@@deriving show, eq]

  let atom x = ProcName_Atom x
  let prod x = ProcName_Prod x

end

module Address =
struct

  type t =
    {
      owner : Types.public_identity;
      pname : Name.t
    }
  [@@deriving show, eq]

  type access_path =
    | Root
    | Access of Name.t * access_path
  [@@deriving show, eq]

  type 'a multi_dest = {
    dests : (t * access_path) list;
    msg   : 'a
  }
  [@@deriving show, eq]

  type 'a prov = {
    path : access_path;
    msg  : 'a
  }
  [@@deriving show, eq]

end

module type S =
sig
  module I : sig 
    include Types.Equalable 
    include Types.Showable with type t := t
    val deserialize : string -> Address.access_path -> t
  end
  module O : sig
    include Types.Equalable 
    include Types.Showable with type t := t
    val serialize : t -> string
  end
  type state
  val show_state : state -> string
  val name   : Name.t
  val thread : (state, I.t, O.t Address.multi_dest) t
end

type ('a, 'b) process_module = (module S with type I.t = 'a and type O.t = 'b)

let evolve
    (type i o) 
    (m : (i, o) process_module) =
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
          let name   = M.name
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
         let name   = M.name
         let thread = next
       end
       in
       let res = (module R : S with type I.t = i and type O.t = o) in
       Lwt.return (output, res))
  | Stop ->
    Stop

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
