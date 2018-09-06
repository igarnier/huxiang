module PongNode = Huxiang.Node.Make(Pingpong.PingPongForPong)

let _ =
  let open Huxiang in
  let () = Lwt_log.add_rule "*" Lwt_log.Debug in
  Lwt_log.default := (Lwt_log.channel
                        ~template:"[$(level)] $(message)"
                        ~channel:Lwt_io.stderr
                        ~close_mode:`Keep
                        ());  
  PongNode.start
    ~listening:"tcp://127.0.0.1:5557"
    ~network_map:(fun Address.{ owner; _ } -> 
        if Crypto.Public.equal owner Directory.basket_pkey then
          "tcp://127.0.0.1:5558"
        else (* pong *)
          "tcp://127.0.0.1:5556"
      )
    ~skey:Directory.pong_skey
    ~pkey:Directory.pong_pkey

