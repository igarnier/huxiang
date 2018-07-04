open Batteries
open Lwt.Infix

module Json = Yojson.Safe

type message =
  | Json of Yojson.Safe.json
  | Ping of Int64.t
  | Ack  of Int64.t

let write_bytes socket bytes =
  let bytes = Bytes.of_string bytes in
  try%lwt Lwt_unix.write socket bytes 0 (Bytes.length bytes)
  with
  | exn ->
    Lwt_io.eprint "huxiang: error caught in write_bytes\n";%lwt
    Lwt.fail exn

let rec really_read fd buffer start length =
  if length <= 0 then
    Lwt.return ()
  else
    Lwt_unix.read fd buffer start length >>= function
    | 0 -> Lwt.fail End_of_file
    | r -> really_read fd buffer (start + r) (length - r)

let read_bytes_until_cr socket =
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec loop acc =
    let len = Bytes.length acc in
    if len > 0 && Bytes.get acc (len - 1) = '\r' then
      Lwt.return (Bytes.sub buffer 0 (len - 1))
    else
      Lwt_unix.read socket buffer 0 buffer_size >>= fun n ->
      let acc = Bytes.cat acc (Bytes.sub buffer 0 n) in
      loop acc
  in
  try%lwt
    loop Bytes.empty
  with
  | exn ->
    Lwt_io.eprint "huxiang: error caught in read_bytes_until_cr\n";%lwt
    Lwt.fail exn

let close socket =
  Lwt.return (Lwt_unix.shutdown socket Unix.SHUTDOWN_ALL)

let read_message socket =
  let%lwt bytes = read_bytes_until_cr socket in
  let     str   = Bytes.to_string bytes in
  if String.length str < 4 then
    Lwt.fail (Failure "huxiang: read_message: input message too short")
  else
    let headr = String.head str 4 in
    match headr with
    | "ping" ->
      let data = Int64.of_string (String.tail str 4) in
      Lwt.return (Ping data)
    | "ackn"  ->
      let data = Int64.of_string (String.tail str 4) in
      Lwt.return (Ack data)
    | "json" ->
      let json = Json.from_string (String.tail str 4) in
      Lwt.return (Json json)
    | _ ->
      Lwt.fail (Failure "huxiang: read_message: incorrect header")

let write_message socket msg =
  let%lwt bytes =
    match msg with
    | Json json ->
      (try%lwt
         Lwt.return ("json"^(Json.to_string json))
       with
       | exn ->
         Lwt_io.eprint "huxiang: error caught in write_message (Json problem?)\n";%lwt
         Lwt.fail exn)
    | Ping i ->
      Lwt.return @@ "ping"^(Int64.to_string i)
    | Ack i ->
      Lwt.return @@ "ackn"^(Int64.to_string i)
  in
  let     data   = bytes^"\r" in    
  let%lwt nbytes = write_bytes socket data in
  Lwt.return ()

