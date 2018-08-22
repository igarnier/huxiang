open Types

type ('i, 'o) transition =
  | Input of ('i -> 'o)
  | NoInput of 'o

(** The "state" of a machine is in fact divided in two parts:
    the usal part (component [state]) and the input/noinput
    state of the process. *)
type ('s,'i,'o) t = {
  move  : ('s, 'i, 'o) transition_function;
  state : 's
}

and ('s, 'i, 'o) transition_function =
  's -> ('i, ('s, 'i, 'o) outcome Lwt.t) transition list
    
and ('s, 'i, 'o) outcome =
  {
    output : 'o option;
    next   : ('s, 'i, 'o) t
  }

type ('s, 'i, 'o) scheduler =
  ('i, ('s, 'i, 'o) outcome Lwt.t) transition list -> int option

let first_pick_scheduler = function
  | x :: _ -> Some 0
  | _      -> None

let uniform_random_scheduler = function
  | [] -> None
  | l  ->
    let len = List.length l in
    let p   = Random.int len in
    Some p

let eager_random_scheduler transitions =
  match transitions with
  | [] -> None
  | _ ->
    let noinput, input = 
      List.partition (function NoInput _ -> true | _ -> false) transitions
    in
    match uniform_random_scheduler noinput with
    | None ->
      uniform_random_scheduler input
    | res  -> res

module type Scheduler =
sig
  val scheduler : ('s, 'i, 'o) scheduler
end

module type S =
sig

  type input
  type output
  type state

  val show_state : state -> string
  val name   : Name.t
  val thread : (state, input, output) t

end

let with_input_plain (f : 'i -> ('s, 'i, 'o) outcome Lwt.t) =
  Input f

let without_input_plain (x : ('s, 'i, 'o) outcome Lwt.t) =
  NoInput x

let with_input (f : 'i -> ('s, 'i, 'o) outcome Lwt.t) =
  [Input f]

let without_input (x : ('s, 'i, 'o) outcome Lwt.t) =
  [NoInput x]

let evolve { move; state } =
  move state

type ('a, 'b) process_module = 
  (module S with type input = 'a and type output = 'b)

let evolve_module
    (type i o) 
    (m : (i, o) process_module) =
  let module M = (val m) in
  let results  = M.thread.move M.thread.state in
  ListLabels.map results ~f:(fun result ->
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
    )

(* let rec (>>>) p q =
 *   fun state ->
 *     let results = p state in
 *     ListLabels.map results ~f:(fun result ->
 *         match result with
 *         | Input f ->
 *           Input (fun i ->
 *               let%lwt { output; next } = f i in
 *               Lwt.return { output; 
 *                            next = { move  = next.move >>> q; 
 *                                     state = next.state } }
 *             )
 *         | NoInput res ->
 *           NoInput (let%lwt { output; next } = res in
 *                    Lwt.return { output; 
 *                                 next = { move  = next.move >>> q; 
 *                                          state = next.state } })
 *         | Stop ->
 *           q state
 *       ) *)

let stop state = 
  Lwt.return { output = None; 
               next = { move = (fun _ -> []); 
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
    let results = process.move state in
    ListLabels.map results ~f:(fun result ->
        match result with
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
      )
  in proc

let rec postcompose process g =
  let rec proc =
    {
      process with move
    }
  and move state =
    let results = process.move state in
    ListLabels.map results ~f:(fun result ->
        match result with
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
      )
  in proc
