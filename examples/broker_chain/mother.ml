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
  val service : Types.address
  val broker  : Types.address
  val timeout : int64
  val uri     : string
end

module Mother(X : ArgSig) =
struct

  let _ =
    let passphrase = input_password X.account in
    Rpc.Personal.unlock_account ~account:X.account ~uri:"http://localhost:8545" ~passphrase ~unlock_duration:3600

  let _ =
    let passphrase = input_password X.service in
    Rpc.Personal.unlock_account ~account:X.service ~uri:"http://localhost:8545" ~passphrase ~unlock_duration:3600

  let _ =
    let passphrase = input_password X.broker in
    Rpc.Personal.unlock_account ~account:X.broker ~uri:"http://localhost:8545" ~passphrase ~unlock_duration:3600

  (* Compile solidity file using solc with the right options, parse the
     result back. This includes the binary code of the contract and its ABI. *)
  let solidity_output = Compile.to_json ~filename:"mother.sol"

  (* Get the contract address on chain *)
  let deploy_receipt =
    let given_gas = Z.of_int 500000 in
    let receipt =
      Compile.deploy_rpc
        ~uri:X.uri
        ~account:X.account
        ~gas:given_gas
        ~contract:solidity_output
        ~arguments:ABI.([ address_val X.broker; address_val X.service; uint256_val X.timeout ])
        ~value:None
    in
    if Z.equal receipt.Types.Tx.gas_used given_gas then
      failwith "mother: contract could not be deployed"
    else
      receipt


  let ctx_address =
    match deploy_receipt.Types.Tx.contract_address with
    | None ->
      failwith "could not get contract address from deploy receipt"
    | Some addr -> addr

  let ctx =
    match solidity_output.contracts with
    | [] | _ :: _ :: _ ->
      failwith "Storage: more than one contract"
    | [ctx] -> ctx

  let find_method mname =
    Compile.get_method ctx mname

  let service_keepalive =
    let open Lwt in
    let service_keepalive_abi =
      match find_method "service_keepalive"  with
      | None -> failwith "service_keepalive method not found in solidity output"
      | Some abi -> abi
    in
    fun () ->
      let given_gas = Z.of_int 99999 in
      let%lwt receipt =
        Compile.execute_method_lwt
          ~uri:X.uri
          ~abi:service_keepalive_abi
          ~arguments:[]
          ~src:X.service
          ~ctx:ctx_address
          ~gas:(Z.of_int 99999)
          ~value:None
      in
      if Z.equal receipt.Types.Tx.gas_used given_gas then
        Lwt.fail_with "service_keepalive: contract call terminated in an erroneous state"
      else
        Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

  let broker_keepalive =
    let open Lwt in
    let broker_keepalive_abi =
      match find_method "broker_keepalive"  with
      | None -> failwith "broker_keepalive method not found in solidity output"
      | Some abi -> abi
    in
    fun () ->
      let given_gas = Z.of_int 99999 in
      let%lwt receipt =
        Compile.execute_method_lwt
          ~uri:X.uri
          ~abi:broker_keepalive_abi
          ~arguments:[]
          ~src:X.broker
          ~ctx:ctx_address
          ~gas:(Z.of_int 99999)
          ~value:None
      in
      if Z.equal receipt.Types.Tx.gas_used given_gas then
        Lwt.fail_with "service_keepalive: contract call terminated in an erroneous state"
      else
        Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

  let service_deposit =
    let open Lwt in
    let service_deposit_abi =
      match find_method "service_deposit"  with
      | None -> failwith "service_deposit method not found in solidity output"
      | Some abi -> abi
    in
    fun value ->
      let given_gas = Z.of_int 99999 in
      let%lwt receipt =
        Compile.execute_method_lwt
          ~uri:X.uri
          ~abi:service_deposit_abi
          ~arguments:[]
          ~src:X.service
          ~ctx:ctx_address
          ~gas:(Z.of_int 99999)
          ~value:(Some value)
      in
      if Z.equal receipt.Types.Tx.gas_used given_gas then
        Lwt.fail_with "service_deposit: contract call terminated in an erroneous state"
      else
        Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

  let broker_deposit =
    let open Lwt in
    let broker_deposit_abi =
      match find_method "broker_deposit"  with
      | None -> failwith "broker_deposit method not found in solidity output"
      | Some abi -> abi
    in
    fun value ->
      let given_gas = Z.of_int 99999 in
      let%lwt receipt =
        Compile.execute_method_lwt
          ~uri:X.uri
          ~abi:broker_deposit_abi
          ~arguments:[]
          ~src:X.broker
          ~ctx:ctx_address
          ~gas:(Z.of_int 99999)
          ~value:(Some value)
      in
      if Z.equal receipt.Types.Tx.gas_used given_gas then
        Lwt.fail_with "broker_deposit: contract call terminated in an erroneous state"
      else
        Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

end


module MotherProcess(X : ArgSig) =
struct

  open Huxiang

  module M = Mother(X)
  
  module I =
  struct
    type t = [Messages.broker_to_mother | Messages.service_to_mother]
    [@@deriving eq, yojson, show]

    let serialize x =
      Yojson.Safe.to_string (to_yojson x)

    let deserialize s pth =    
      match pth with
      | Process.Address.Root ->
        (match of_yojson (Yojson.Safe.from_string s) with
         | Ok x -> x
         | Error s -> failwith s)
      | _ ->
        failwith "pingmsg/deserialize: wrong path"
  end
  module O = Messages.Nothing

  type state = unit [@@deriving show]

  let name = Process.Name.atom "mother"

  let rec main_loop state =
    Process.with_input (function
        | `BrokerDeposit ->
          Lwt.ignore_result (M.broker_deposit (Z.of_int 1000));
          Lwt_io.printf "broker: deposit successful\n";%lwt
          Process.continue_with state main_loop
        | `ServiceDeposit ->
          Lwt.ignore_result (M.service_deposit (Z.of_int 1000));
          Lwt_io.printf "service: deposit successful\n";%lwt
          Process.continue_with state main_loop
        | `BrokerKeepAlive ->
          let%lwt events = M.broker_keepalive () in
          (match events with
           | [] ->
             Lwt.fail_with "mother/motherprocess/main_loop: no messages written when calling broker_keepalive"
           | _ :: _ :: _ ->
             Lwt.fail_with "mother/motherprocess/main_loop: more that one message written when calling broker_keepalive"
           | [{ event_name; event_args }] ->
             (match event_name, event_args with
              | "ServiceTimeout", [ { desc = ABI.Int delta } ] ->
                let s = 
                  Printf.sprintf
                    "mother/motherprocess/main_loop: service timeout (%Ld) received from mother contract" 
                    delta 
                in
                Lwt.fail_with s
              | "ServiceAlive", [ { desc = ABI.Int delta } ]  ->
                Lwt_io.printf "broker: keepalive successful (%Ld)\n" delta;%lwt
                Process.continue_with state main_loop
              | _ ->
                Lwt.fail_with @@ "mother/motherprocess/main_loop: unknown event "^event_name
             )
          )
        | `ServiceKeepAlive ->
          let%lwt events = M.service_keepalive () in
          (match events with
           | [] ->
             Lwt.fail_with "mother/motherprocess/main_loop: no messages written when calling service_keepalive"
           | _ :: _ :: _ ->
             Lwt.fail_with "mother/motherprocess/main_loop: more that one message written when calling service_keepalive"
           | [{ event_name; event_args }] ->
             (match event_name, event_args with
              | "BrokerTimeout", [ { desc = ABI.Int delta } ] ->
                let s = 
                  Printf.sprintf
                    "mother/motherprocess/main_loop: broker timeout (%Ld) received from mother contract" 
                    delta 
                in
                Lwt.fail_with s
              | "BrokerAlive",  [ { desc = ABI.Int delta } ] ->
                Lwt_io.printf "service: keepalive successful (%Ld)\n" delta;%lwt
                Process.continue_with state main_loop
              | _ ->
                Lwt.fail_with @@ "mother/motherprocess/main_loop: unknown event "^event_name
             )
          )
      )

  let thread =
    {
      Process.move  = main_loop;
      Process.state = ()
    }

  
end


(* for simplicity, deploy the mother contract with the same account that plays as "service". In reality, this could
   be e.g. a "smarthab" account, with payement done either on-chain or in fiat by the parties. *)
module Mom =
  MotherProcess(struct 
    let account = Types.address_from_string "0xa742250854eeabb8a4165ee5fd1f5f76c69bf053"
    let service = Types.address_from_string "0xa742250854eeabb8a4165ee5fd1f5f76c69bf053"
    let broker  = Types.address_from_string "0x7beb9484e091bccd84ffa9a1fda0ba59092c1f79"
    let timeout = 10L
    let uri     = "http://localhost:8545"
  end)

let _ =
  let module MomNode = Huxiang.Node.Make(Mom) in
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  MomNode.start_dynamic
    ~listening:Directory.service_mother
    ~network_map:Directory.network_map

