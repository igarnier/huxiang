# huxiang 互相
A library for functorially defined communicating state machines over TCP.

* INSTALLATION

The following instructions allow to install the `huxiang` library.

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

FAQ: why the name? I find the character 互 very cool.
