type identity = Left | Right

module type Selector =
sig
  val selector : identity
end

module Prod : functor
  (Left : Types.Process)
  (Right : Types.Process)
  (Leader : Types.Leadership)
  (S : Selector) ->
  sig

    module I :
      sig

        type notification

        type t =
            Leader of Leader.t
          | InputForLeft of Left.I.t
          | InputForRight of Right.I.t
          | Notification of notification
        val equal : t -> t -> Ppx_deriving_runtime.bool
        val to_yojson : t -> Yojson.Safe.json
        val of_yojson :
          Types.json -> t Ppx_deriving_yojson_runtime.error_or
        val of_yojson_exn : Types.json -> t
      end

    module O :
      sig
        type t =
            OutputFromLeft of Left.O.t
          | OutputFromRight of Right.O.t
          | Notification of I.notification
        val equal : t -> t -> Ppx_deriving_runtime.bool
        val to_yojson : t -> Yojson.Safe.json
        val of_yojson :
          Types.json -> t Ppx_deriving_yojson_runtime.error_or
        val of_yojson_exn : Types.json -> t
      end

    type state

    val initial_state : state

    val process : (state, I.t, O.t) Types.t

  end
