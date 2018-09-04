open Batteries

open Ocaml_geth
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

let input_password (account : Types.Address.t) =
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
  val account : Types.Address.t
  val service : Types.Address.t
  val broker  : Types.Address.t
  val timeout : int64
  val uri     : string
end

module Mother(X : ArgSig) =
struct

  let _ =
    let passphrase = input_password X.account in
    Rpc.Personal.unlock_account
      ~account:X.account
      ~uri:"http://localhost:8545" 
      ~passphrase 
      ~unlock_duration:3600

  let _ =
    let passphrase = input_password X.service in
    Rpc.Personal.unlock_account
      ~account:X.service 
      ~uri:"http://localhost:8545" 
      ~passphrase 
      ~unlock_duration:3600

  let _ =
    let passphrase = input_password X.broker in
    Rpc.Personal.unlock_account
      ~account:X.broker 
      ~uri:"http://localhost:8545" 
      ~passphrase 
      ~unlock_duration:3600

  (* Compile solidity file using solc with the right options, parse the
     result back. This includes the binary code of the contract and its ABI. *)
  let solidity_output = Compile.to_json ~filename:"mother.sol"

  (* Get the contract address on chain *)
  let deploy_receipt =
    let given_gas = Z.of_int 500000 in
    let arguments =
      ABI.([ address_val X.broker; 
             address_val X.service; 
             uint256_val X.timeout ])
    in
    let receipt =
      Compile.deploy_rpc
        ~uri:X.uri
        ~account:X.account
        ~gas:given_gas
        ~contract:solidity_output
        ~arguments
        ~value:None
    in
    if Z.equal receipt.Types.Tx.gas_used given_gas then
      failwith "mother: contract could not be deployed"
    else
      let receipt_str = Types.Tx.show_receipt receipt in
      Printf.printf "mother: contract deployed; receipt:\n%s\n" receipt_str;
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
        let msg =
          "service_keepalive: contract call terminated in an erroneous state"
        in
        Lwt.fail_with msg
      else
        Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

  let broker_keepalive =
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
        let msg =
          "service_keepalive: contract call terminated in an erroneous state"
        in
        Lwt.fail_with msg
      else
        Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

  let service_deposit =
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
        let msg =
          "service_deposit: contract call terminated in an erroneous state"
        in
        Lwt.fail_with msg
      else
        Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

  let broker_deposit =
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
        let msg =
          "broker_deposit: contract call terminated in an erroneous state"
        in
        Lwt.fail_with msg
      else
        Lwt.return (ABI.Decode.decode_events ctx.abi receipt)

end


module MotherProcess(X : ArgSig) =
struct

  open Huxiang

  module M = Mother(X)
  
  module I =
  struct
    type t = 
      | FromBroker  of Messages.BrokerToMother.t
      | FromService of Messages.ServiceToMother.t
    [@@deriving eq, show, bin_io]
  end

  module O = Messages.Nothing

  type input  = I.t
  type output = O.t Address.multi_dest

  type state = unit [@@deriving show]

  let name = Name.atom "mother"

  let fail where msg =
    Lwt.fail_with @@ where^ ": "^msg

  let rec main_loop state =
    let fname = "mother/motherprocess/main_loop" in
    Process.with_input (function
        | I.FromBroker Deposit ->
          Lwt.ignore_result (M.broker_deposit (Z.of_int 1000));
          Lwt_io.printf "broker: deposit successful\n";%lwt
          Process.continue_with state main_loop
        | I.FromService Deposit ->
          Lwt.ignore_result (M.service_deposit (Z.of_int 1000));
          Lwt_io.printf "service: deposit successful\n";%lwt
          Process.continue_with state main_loop
        | I.FromBroker KeepAlive ->
          let%lwt events = M.broker_keepalive () in
          (match events with
           | [] ->
             let msg = "no messages written when calling broker_keepalive" in
             fail fname msg
           | _ :: _ :: _ ->
             let msg =
               "more that one message written when calling broker_keepalive"
             in
             fail fname msg
           | [{ event_name; event_args }] ->
             (match event_name, event_args with
              | "ServiceTimeout", [ { desc = ABI.Int delta; _ } ] ->
                let msg = 
                  Printf.sprintf
                    "service timeout (%Ld) received from mother contract" 
                    delta 
                in
                fail fname msg
              | "ServiceAlive", [ { desc = ABI.Int delta; _ } ]  ->
                Lwt_io.printf "broker: keepalive successful (%Ld)\n" delta;%lwt
                Process.continue_with state main_loop
              | _ ->
                fail fname ("unknown event "^event_name)
             )
          )
        | I.FromService KeepAlive ->
          let%lwt events = M.service_keepalive () in
          (match events with
           | [] ->
             let msg =
               "no messages written when calling service_keepalive"
             in
             fail fname msg
           | _ :: _ :: _ ->
             let msg =
               "more that one message written when calling service_keepalive"
             in
             fail fname msg
           | [{ event_name; event_args }] ->
             (match event_name, event_args with
              | "BrokerTimeout", [ { desc = ABI.Int delta; _ } ] ->
                let msg =
                  Printf.sprintf
                    "broker timeout (%Ld) received from mother contract" 
                    delta 
                in
                fail fname msg
              | "BrokerAlive",  [ { desc = ABI.Int delta; _ } ] ->
                Lwt_io.printf "service: keepalive successful (%Ld)\n" delta;%lwt
                Process.continue_with state main_loop
              | _ ->
                fail fname ("unknown event "^event_name)
             )
          )
      )

  let thread =
    {
      Process.move  = main_loop;
      Process.state = ()
    }
  
end

(* for simplicity, deploy the mother contract with the same account that plays
   as "service". In reality, this could be e.g. a "smarthab" account, with
   payement done either on-chain or in fiat by the parties. *)
module Mom =
  MotherProcess(struct 
    let account = 
      Types.Address.from_string "0xa742250854eeabb8a4165ee5fd1f5f76c69bf053"
    let service = 
      Types.Address.from_string "0xa742250854eeabb8a4165ee5fd1f5f76c69bf053"
    let broker  = 
      Types.Address.from_string "0x7beb9484e091bccd84ffa9a1fda0ba59092c1f79"
    let timeout = 10L
    let uri     = "http://localhost:8545"
  end)

open Huxiang

let compiled_mom =
  NetProcess.compile (function
      | None ->
        failwith "no public key"
      | Some pkey ->
        if Crypto.Public.equal pkey Directory.BrokerCred.public_key then
          let inject x = Mom.I.FromBroker x in
          Utils.map_reader Messages.BrokerToMother.bin_reader_t inject
        else if Crypto.Public.equal pkey Directory.ServiceCred.public_key then
          let inject x = Mom.I.FromService x in
          Utils.map_reader Messages.ServiceToMother.bin_reader_t inject
        else
          failwith "unknown public key"
    ) Messages.Nothing.bin_writer_t (module Mom)

module MomCred = (val Crypto.(key_pair_to_cred (seeded_key_pair "mother")))

let _ =
  Ocaml_geth.Rpc.switch_debug ();
  let module MomNode = Huxiang.Node.Make((val compiled_mom)) in
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());
  MomNode.start
    ~listening:Directory.mother
    ~network_map:Directory.network_map
    ~skey:Directory.MomCred.secret_key
    ~pkey:Directory.MomCred.public_key

