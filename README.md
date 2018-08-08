# huxiang 互相
A library for functorially defined communicating state machines over TCP.

For now, this code should certainly not be used for purposes beyond 
experimenting. The library provides a functor `Node.Make` parameterized by a
"process". A process specifies
1) which messages can be sent and received, and
2) a transition function from input to outputs.
Given a module of type `Process`, the `Node.Make` functor returns a module
for deploying an actual network node, ready to communicate and implement
the transition function.

Concretely, the messages are serialized to JSON and transported via zeromq on
ports chosen when deploying via Node.make. Requests and outputs are treated
asynchronously via Lwt.

Another, more experimental feature, of the library is the coalescent product
construction, which allows to compute an a process acting as an interpreter 
for the asynchronous product of two processes. When replicated, this allows
parties to form self-monitoring nodes, in a fashion not unsimilar to the idea
of smart contracts.

FAQ: why the name? I find the character 互 very cool.
