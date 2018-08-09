# huxiang 互相
A library and protocol for functorially defined communicating state machines over TCP.

* INSTALLATION

The following instructions allow to install the `huxiang` library. They have been tested
on Ubuntu 18.04.

1) You will need an `OCaml` environment. Install `opam` by following instructions at https://opam.ocaml.org/blog/opam-2-0-0-rc4/
2) Switch to OCaml 4.06.0: `opam switch create 4.06.0`. If needed, update shell environment: `eval $(opam env)`.
3) Update package list: `opam update`.
4) Install `libsodium-dev` and `libzmq3-dev` using your distribution's package manager.
5) Install the following packages using the command `opam install pkg-name`: 
   `batteries`, `sodium`, `yojson`, `lwt`, `lwt_ppx`, `lwt_log`, `zmq`, `ppx_deriving`, 
   `ppx_deriving_yojson`, `zmq-lwt`
6) Clone this repository.
7) Build the library and install it using: `jbuilder build && jbuilder install`.

* EXAMPLES

The `examples/' subdirectory contain several examples. Each should provide a README detailing how
to execute it.

Some of these examples rely on having further dependencies installed. You will need to install
the Geth Ethereum client (https://github.com/ethereum/go-ethereum/wiki/geth) in order to run a 
private Ethereum blockchain. We assume proficiency with that client: you should be able to create 
accounts and mine some Ether locally on those. We provide a `genesis.json` file in the `examples/geth`
directory for convenience.

Those examples interact with the `geth` client using the `ocaml-geth` library, which you should clone
from https://github.com/igarnier/ocaml-geth, build and install using the instructions available on
this repo.
