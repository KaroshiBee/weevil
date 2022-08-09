let _DEFAULT_LOG_FILE = "weevil_stepper.log"
let _DEFAULT_LISTEN_ADDRESS = "loopback"
let _DEFAULT_PORT = 9000

let _replace input output =
  Str.global_replace (Str.regexp_string input) output

let _HEADER_TOKEN = "\r\n\r\n"

let wrap_header js =
  let open Data_encoding.Json in
  let s = js
  |> to_string
  |> _replace "\n" ""
  in
  let n = String.length s in
  Printf.sprintf "Content-Length: %d%s%s" n _HEADER_TOKEN s

let strip_header s =
  let rgx = Str.regexp_string _HEADER_TOKEN in
  match Str.bounded_split rgx s 2 with
  | [] -> Error (Printf.sprintf "No header in %s" s)
  | _ :: js -> Ok (String.concat "" js)
