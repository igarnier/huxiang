open Batteries

open Ocaml_geth
open Basic (* for Bits and Bytes *)
open Contract

(* --------------------------------------------------------------------- *)
(* Some helpful functions *)
(* --------------------------------------------------------------------- *)

(* Read password from stdin in a stealthy manner *)
let read_secret () =
  let open Unix in
  let term_init = tcgetattr stdin in
  let term_no_echo = { term_init with c_echo = false } in
  tcsetattr stdin TCSADRAIN term_no_echo;
  let password =
    try read_line ()
    with _ ->
      (tcsetattr stdin TCSAFLUSH term_init;
       failwith "read_secret: readline failed")
  in
  tcsetattr stdin TCSAFLUSH term_init;
  password

let input_password (account : Types.address) =
  Printf.printf "password for account %s: %!" (account :> string);
  let res = read_secret () in
  print_newline ();
  res

(* --------------------------------------------------------------------- *)
(* Deploying a smart contract. We functorize the code over some global
   parameters (like the creating account). *)
(* --------------------------------------------------------------------- *)

module type ArgSig =
sig
  val account : Types.address
  val uri     : string
end

module Ping(X : ArgSig) =
struct

  let _ =
    let passphrase = input_password X.account in
    Rpc.Personal.unlock_account ~account:X.account ~uri:"http://localhost:8545" ~passphrase ~unlock_duration:3600

  (* Compile solidity file using solc with the right options, parse the
     result back. This includes the binary code of the contract and its ABI. *)
  let solidity_output = Compile.to_json ~filename:"ping.sol"

  (* Get the contract address on chain *)
  let deploy_receipt =
    let given_gas = Z.of_int 175000 in
    let receipt =
      Compile.deploy_rpc
        ~uri:X.uri
        ~account:X.account
        ~gas:given_gas
        ~contract:solidity_output
        ~arguments:[]
        ~value:None
    in
    if Z.equal receipt.Types.Tx.gas_used given_gas then
      failwith "ping: contract could not be deployed"
    else
      receipt

 
  let ctx_address =
    match deploy_receipt.Types.Tx.contract_address with
    | None ->
      failwith "could not get contract address from deploy receipt"
    | Some addr -> addr

  let _ =
    let code = Rpc.Eth.get_code ~uri:X.uri ~address:ctx_address ~at_time:`latest in
    if code = "0x" then
      failwith "Ping: deployment failed - check constructor typing"

  (* --------------------------------------------------------------------- *)
  (* Calling a method from a solidity smart contract *)
  (* --------------------------------------------------------------------- *)

  let ctx =
    match solidity_output.contracts with
    | [] | _ :: _ :: _ ->
      failwith "Storage: more than one contract"
    | [ctx] -> ctx

  let find_method mname =
    Compile.get_method ctx mname
      
  let recv_pong =
    let open Lwt in
    let recv_pong_abi =
      match find_method "recv_pong"  with
      | None -> failwith "recv_pong method not found in solidity output"
      | Some abi -> abi
    in
    fun i ->
      Compile.execute_method_lwt
        ~uri:X.uri
        ~abi:recv_pong_abi
        ~arguments:[ABI.uint256_val i]
        ~src:X.account
        ~ctx:ctx_address
        ~gas:(Z.of_int 99999)
      >>= fun receipt ->
      Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

end

module type PingSig =
sig
  val recv_pong : int64 -> (Contract.ABI.event list) Lwt.t
end

module Protocol(X : ArgSig) : Huxiang.Process.S =
struct

  open Huxiang

  module I = Messages.ServerToClient
  module O = Messages.ClientToServer

  module P : PingSig = Ping(X)

  type state = unit

  let show_state _ = "()"

  let name = Process.Name.atom "ping"

  let rec main_loop state =
    Process.with_input (fun (I.Pong { block_id }) ->
      let%lwt events = P.recv_pong block_id in
      match events with
      | [] ->
        Lwt.fail_with "transition: no messages written when calling recv_pong"
      | [{ event_name; event_args }] ->
        (match event_name, event_args with
         | "PingEvent", [ { desc = ABI.Int uid }  ] ->
           let output = Process.(O.Ping { uid } @ Directory.pong_node) in
           Process.continue_with ~output state main_loop
         | "ErrorDetected", _ ->
           Process.stop state
         | _ ->
           Lwt.fail_with "Ping.transition: wrong event"
        )
      | _ ->
        Lwt.fail_with "Ping.transition: more than one event"
      )

  let process state =
    let output = Process.(O.Ping { uid = 0L } @ Directory.pong_node) in
    Process.without_input
      (Process.continue_with ~output state main_loop)

  let thread =
    {
      Process.move  = process;
      Process.state = ()
    }

end

(* let _ = Rpc.switch_debug () *)

module Pro =
  Protocol(struct 
    let account = Types.address_from_string "0x7beb9484e091bccd84ffa9a1fda0ba59092c1f79"
    let uri     = "http://localhost:8545"
  end)

module Client = Huxiang.Node.Make(Pro)

let _ =
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  Client.start_dynamic
    ~listening:"tcp://127.0.0.1:5555"
    ~network_map:(fun _ -> "tcp://127.0.0.1:5556")
    
