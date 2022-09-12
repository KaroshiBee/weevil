open Data_encoding__.Json

let _DEFAULT_LOG_FILE = "weevil_stepper.log"
let _DEFAULT_LISTEN_ADDRESS = "loopback"
let _DEFAULT_PORT = 9000
let _DEFAULT_BACKLOG = 10

let _THE_THREAD_ID = 1
let _THE_FRAME_ID = 1
let _THE_FRAME_NAME = "main" (* TODO would probably be the method name or contract entry point name *)
let _THE_ONLY_SCOPE = ("Locals", 1) (* suggested -> | Arguments | Locals | Registers *)
let _THE_GAS_LOCAL = ("gas", 0)
let _THE_MICHELSON_STACK_LOCAL = ("stack", 1)

let _replace input output =
  Str.global_replace (Str.regexp_string input) output

let _HEADER_FIELD = "Content-Length: "
let _HEADER_TOKEN = "\r\n\r\n"

let wrap_header js =
  (* let open Data_encoding.Json in *)
  let s = js
  |> to_string
  |> _replace "\n" ""
  in
  let n = String.length s in
  Printf.sprintf "%s%d%s%s" _HEADER_FIELD n _HEADER_TOKEN s

let strip_header s =
  let rgx = Str.regexp_string _HEADER_TOKEN in
  match Str.bounded_split rgx s 2 with
  | [] -> Error (Printf.sprintf "No header in %s" s)
  | _ :: js -> Ok (String.concat "" js)


let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try
      ignore (Str.search_forward re s1 0);
      true
    with Not_found -> false
