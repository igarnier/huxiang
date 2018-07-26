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
  val partner : Types.address
  val uri     : string
end

module Pong(X : ArgSig) =
struct

  let _ =
    let passphrase = input_password X.account in
    Rpc.Personal.unlock_account ~account:X.account ~uri:"http://localhost:8545" ~passphrase ~unlock_duration:3600

  (* Compile solidity file using solc with the right options, parse the
     result back. This includes the binary code of the contract and its ABI. *)

  let solidity_output = Compile.to_json ~filename:"pong.sol"

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
      failwith "pong: contract could not be deployed"
    else
      receipt
 
  let ctx_address =
    match deploy_receipt.Types.Tx.contract_address with
    | None ->
      failwith "could not get contract address from deploy receipt"
    | Some addr -> addr

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

  let recv_ping =
    let open Lwt in
    let recv_ping_abi =
      match find_method "recv_ping"  with
      | None -> failwith "recv_ping method not found in solidity output"
      | Some abi -> abi
    in
    fun i ->
      Compile.execute_method_lwt
        ~uri:X.uri
        ~abi:recv_ping_abi
        ~arguments:[ABI.uint256_val i]
        ~src:X.account
        ~ctx:ctx_address
        ~gas:(Z.of_int 99999)
        ~value:None
      >>= fun receipt ->
      Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

end

module type PongSig =
sig
  val recv_ping : int64 -> (Contract.ABI.event list) Lwt.t
end

module Protocol(X : ArgSig) : Huxiang.Process.S =
struct

  open Huxiang

  module I = Messages.ClientToServer
  module O = Messages.ServerToClient

  module P : PongSig = Pong(X)

  type state = ()

  let show_state _ = "()"

  let name = Process.Name.atom "ping"

  let rec main_loop (state : state) =
    Process.with_input (fun (I.Ping { uid }) ->
        let%lwt events = P.recv_ping uid in
        match events with
        | [] ->
          Lwt.fail (Failure "transition: no messages written when calling recv_ping")
        | [{ event_name; event_args }] ->
          (match event_name, event_args with
           | "PongEvent", [ { desc = ABI.Int block_id }  ] ->
             (* | "PongEvent", [   ] -> *)
             let output = {
               Process.Address.msg = O.Pong { block_id };
               dests = [ (Directory.ping_node, Root) ]
             } in
             Process.continue_with ~output state main_loop
           | _ ->
             failwith "Ping.transition: wrong event"
          )
        | _ ->
          failwith "Ping.transition: more than one event"
      )

  let thread =
    {
      Process.move  = main_loop;
      Process.state = ()
    }


end

let _ =
  (* Rpc.switch_debug (); *)
  let module Pro    = Protocol(
    struct 
      let account = Types.address_from_string "0xa742250854eeabb8a4165ee5fd1f5f76c69bf053"
      let uri     = "http://localhost:8545"
      let partner = Types.address_from_string "0x7beb9484e091bccd84ffa9a1fda0ba59092c1f79"
    end) in
  let module Server = Huxiang.Node.Make(Pro) in
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  Server.start_dynamic
    ~listening:"tcp://127.0.0.1:5556"
    ~network_map:(fun _ -> "tcp://127.0.0.1:5555")
