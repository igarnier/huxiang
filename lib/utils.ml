open Batteries
open Lwt.Infix

let write_bytes socket bytes =
  let bytes = Bytes.of_string bytes in
  Lwt_unix.write socket bytes 0 (Bytes.length bytes)

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
  Lwt.catch (fun () ->
      loop Bytes.empty
    )
    (fun exn ->
       Lwt_io.eprint "huxiang: error caught in read_bytes_until_cr\n" >>= fun () ->
       Lwt.fail exn
    )
