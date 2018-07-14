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

        include Types.Jsonable  with type t := t
        include Types.Equalable with type t := t
        include Types.Showable  with type t := t

      end

    module O :
      sig
        type t =
            OutputFromLeft of Left.O.t
          | OutputFromRight of Right.O.t
          | Notification of I.notification

        include Types.Jsonable  with type t := t
        include Types.Equalable with type t := t
        include Types.Showable  with type t := t

      end

    type state

    val show_state : state -> string

    val initial_state : state

    val process : (state, I.t, O.t) Types.t

  end
