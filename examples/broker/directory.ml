open Huxiang


module ClientCred = (val (Crypto.(key_pair_to_cred (seeded_key_pair "client"))))

let client_node =
  {
    Address.owner = ClientCred.public_key;
    pname = Name.atom "client"
  }

module ServiceCred = (val (Crypto.(key_pair_to_cred (seeded_key_pair "service"))))

let service_node = 
  {
    Address.owner = ServiceCred.public_key;
    pname = Name.atom "service"
  }

module BrokerCred = (val (Crypto.(key_pair_to_cred (seeded_key_pair "broker"))))

let broker_node =
  {
    Address.owner = BrokerCred.public_key;
    pname = Name.atom "broker"
  }

let clnt = "tcp://127.0.0.1:5555"
let brok_clnt = "tcp://127.0.0.1:5557"
let brok_serv = "tcp://127.0.0.1:5558"
let serv = "tcp://127.0.0.1:5556"

