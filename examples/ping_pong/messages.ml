module Json = Yojson.Safe

module ClientToServer =
struct

  type client_id = string

  type t =
    | Hello of { client_id : client_id; nonce : int }
    | Alive of { client_id : client_id; nonce : int }

  let to_json (message : t) : Json.json =
    match message with
    | Hello { client_id; nonce } ->
      `Variant("Hello", Some (`List [`String client_id; `Int nonce ]))
    | Alive { client_id; nonce } ->
      `Variant("Alive", Some (`List [`String client_id; `Int nonce ]))

  let from_json (j : Json.json) : t =
    match j with
    | `Variant("Hello", Some (`List [`String client_id; `Int nonce ])) ->
      Hello { client_id; nonce }
    | `Variant("Alive", Some (`List [`String client_id; `Int nonce ])) ->
      Alive { client_id; nonce }
    | _ ->
      let m =
        Printf.sprintf "from_json: incorrect input json %s\n"
          (Json.to_string j)
      in
      failwith m

end

module ServerToClient =
struct

  type t =
    | ServerStartup
    | Ack of ClientToServer.t
    | Die

  let to_json (message : t) : Json.json =
    match message with
    | ServerStartup ->
      `Variant("ServerStartup", None)
    | Ack msg ->
      `Variant("Ack", Some (ClientToServer.to_json msg))
    | Die ->
      `Variant("Die", None)

  let from_json (j : Json.json) : t =
    match j with
    | `Variant("ServerStartup", None) ->
      ServerStartup
    | `Variant("Ack", Some msg) ->
      Ack (ClientToServer.from_json msg)
    | `Variant("Die", None) ->
      Die
    | _ ->
      let m =
        Printf.sprintf "from_json: incorrect input json %s\n"
          (Json.to_string j)
      in
      failwith m

end
