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
  's -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition

and ('s, 'i, 'o) outcome =
  {
    output : 'o option;
    next   : ('s, 'i, 'o) t
  }

module type S =
sig

  type input
  type output

  type state
  val show_state : state -> string
  val name   : Name.t
  val thread : (state, input, output) t

end

type ('a, 'b) process_module = (module S with type input = 'a and type output = 'b)

let with_input (f : 'i -> ('s, 'i, 'o) outcome Lwt.t) =
  Input f

let without_input (x : ('s, 'i, 'o) outcome Lwt.t) =
  NoInput x

let evolve
    (type i o) 
    (m : (i, o) process_module) =
  let module M = (val m) in
  let result = M.thread.move M.thread.state in
  match result with
  | Input f ->
    Input (fun i ->
        let%lwt { output; next } = f i in
        let module R : S with type input = i and type output = o =  
        struct
          type input  = M.input
          type output = M.output
          type state  = M.state
          let show_state = M.show_state
          let name   = M.name
          let thread = next
        end
        in
        let res = (module R : S
                    with type input = i 
                     and type output = o)
        in
        (Lwt.return (output, res))
      )
  | NoInput result ->
    NoInput
      (let%lwt { output; next } = result in
       let module R : S with type input = i and type output = o =  
       struct
         type input  = M.input
         type output = M.output
         type state  = M.state
         let show_state = M.show_state
         let name   = M.name
         let thread = next
       end
       in
       let res = (module R : S with type input = i and type output = o) in
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

let stop state = 
  Lwt.return { output = None; 
               next = { move = (fun _ -> Stop); 
                        state } 
             }

let continue_with ?output state move =
  Lwt.return { output;
               next   = { move; state } }

(* val sequence : ('s, 'i, 'o) t -> ('t, 'o, 'p) t -> ('s * 't, 'i, 'p) t *)

(* let rec sequence p1 p2 =
 *   let rec proc =
 *     {
 *       state = (p1.state, p2.state);
 *       move
 *     }
 *   and move (st1, st2) =
 *     match p2.move st2 with
 *     | Stop -> Stop
 *     | NoInput result ->
 *       NoInput
 *         (let%lwt r = result in
 *          Lwt.return {
 *            r with
 *            next = sequence p1 r.next
 *          }
 *         )
 *     | Input f2 ->
 *       begin match p1.move st1 with
 *         | Stop -> Stop
 *         | NoInput result ->    
 *           NoInput
 *             (let%lwt r = result in
 *              (match r.output with
 *               | Some o ->
 *                 (let%lwt r = f2 o in
 *                  Lwt.return {
 *                    r with
 *                    next = sequence p1.next p2.next
 *                  })
 *               | None ->
 *                 failwith "")
 *             )
 *         | Input f1 ->
 *           Input (fun i ->
 *               failwith ""
 *             )
 *       end *)

let rec precompose process g =
  let rec proc =
    {
      process with move
    }
  and move state =
    match process.move state with
    | Input f ->
      Input (fun j ->
          let%lwt r = f (g j) in
          Lwt.return { r with
            next = precompose r.next g
          }
        )
    | NoInput result ->
      NoInput 
        (let%lwt r = result in
         Lwt.return {
           r with next = precompose r.next g
         })
    | Stop -> Stop
  in proc

let rec postcompose process g =
  let rec proc =
    {
      process with move
    }
  and move state =
    match process.move state with
    | Input f ->
      Input (fun j ->
          let%lwt r = f j in
          Lwt.return { 
            output = Batteries.Option.map g r.output;
            next = postcompose r.next g
          }
        )
    | NoInput result ->
      NoInput 
        (let%lwt r = result in
         Lwt.return {
           output = Batteries.Option.map g r.output;
           next = postcompose r.next g
         })
    | Stop -> Stop
  in proc
