(** A network node is a small server executing a communicating state machine.
    It can have several sockets on input and output. Sockets can be of two
    types: subscribe/publish (sub/pub) or reliable. In sub/pub mode, nodes
    on the receiving end subscribe to a flux of messages emitted by another
    node. A "Publish" socket can only be connected to (possibly several)
    "Subscribe" ones. Note that is there are no subscribers, messages emitted
    will be lost.
    
    Reliable sockets on the other hand never lose messages. Messages emitted
    on a "ReliableOut" socket will be queued until a peer connects (with
    a "ReliableIn" socket). Writing to a "ReliableOut" socket can block if
    the queue is full.

    It is up to each node to correctly setup socket types matching that of
    network peers.
*)

type address = string

type input_socket =
  | Subscribe  of address
  | ReliableIn of address

type output_socket =
  | Publish     of address
  | ReliableOut of address

type network_map = Address.t -> output_socket

module Make(P : NetProcess.S) :
sig

  val start : 
    listening:input_socket list -> 
    network_map:network_map ->
    skey:Crypto.Secret.t ->
    pkey:Crypto.Public.t ->
    unit  
  
end
