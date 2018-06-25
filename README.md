# huxiang 互相
A library for simple communicating state machines over TCP.



This code should certainly not be used for purposes beyond experimenting.
The library provides two functors: Client.Make and Server.Make, both parameterized
by a protocol. A protocol specifies
1) which messages can be sent from client to server and back, and
2) message-labelled transition tables for the clients and the server.

The transition table is defined coalgebraically. In particular, the states of the
clients and servers are hidden from each other. In [huxiang], the server state is
not replicated per-client but shared: from the point of view of the server, all
messages could originate from one unique client. Of course, this doesn't prevent
client-specific behaviour, but this has to be programmed at the protocol level.

Concretely, the messages are serialized to JSON and transported via TCP on a
port chosen by the user of the library.